# vi: fdm=marker

#' @include biodb-common.R
#' @include MassdbConn.R 

# In the provided file, each line represents an MS peak measure.
# The file contains molecule and spectrum information. Each spectrum has an accession id.

# Constants {{{1
################################################################

# Default database fields
.BIODB.DFT.DB.FIELDS <- c('accession', 'name', 'fullnames', 'compound.id', 'ms.level', 'ms.mode', 'peak.mzexp', 'peak.mztheo', 'peak.relative.intensity', 'peak.comp', 'peak.attr', 'chrom.col', 'chrom.col.rt', 'formula', 'mass', 'inchi', 'inchikey', 'chebi.id', 'hmdb.metabolite.id', 'kegg.compound.id', 'ncbi.pubchem.comp.id')
names(.BIODB.DFT.DB.FIELDS) <- .BIODB.DFT.DB.FIELDS

# Class declaration {{{1
################################################################

MassCsvFileConn <- methods::setRefClass("MassCsvFileConn", contains = "MassdbConn", fields = list(.file.sep = "character", .file.quote = "character", .field.multval.sep = 'character', .db = "ANY", .db.orig.colnames = "character", .fields = "character", .ms.modes = "character", .precursors = "character"))

# Constructor {{{1
################################################################

MassCsvFileConn$methods( initialize = function(file.sep = "\t", file.quote = "\"", ...) {

	callSuper(content.type = BIODB.TSV, ...)

	# Check file
	if ( ! file.exists(.self$getBaseUrl()))
		.self$message('error', paste("Cannot locate the file database \"", .self$getBaseUrl() ,"\".", sep = ''))

	# Set fields
	.db <<- NULL
	.db.orig.colnames <<- NA_character_
	.file.sep <<- file.sep
	.file.quote <<- file.quote
	.fields <<- .BIODB.DFT.DB.FIELDS
	.field.multval.sep <<- ';'
	.ms.modes <<- BIODB.MSMODE.VALS
	names(.self$.ms.modes) <- BIODB.MSMODE.VALS

	# Precursors
	.precursors <<- c("[(M+H)]+", "[M+H]+", "[(M+Na)]+", "[M+Na]+", "[(M+K)]+", "[M+K]+", "[(M-H)]-", "[M-H]-", "[(M+Cl)]-", "[M+Cl]-")
})

# Is valid field tag {{{1
################################################################

MassCsvFileConn$methods( isValidFieldTag = function(tag) {
	return (tag %in% names(.self$.fields))
})

# Init db {{{1
################################################################

MassCsvFileConn$methods( .init.db = function() {

	if (is.null(.self$.db)) {

		# Load database
		.db <<- read.table(.self$getBaseUrl(), sep = .self$.file.sep, quote = .self$.file.quote, header = TRUE, stringsAsFactors = FALSE, row.names = NULL)

		# Save column names
		.db.orig.colnames <<- colnames(.self$.db)
	}
})

# Set field {{{1
################################################################

MassCsvFileConn$methods( setField = function(tag, colname) {

	tag <- tolower(tag)

	( ! is.null(tag) && ! is.na(tag)) || .self$message('error', "No tag specified.")
	( ! is.null(colname) && ! is.na(colname)) || .self$message('error', "No column name specified.")

	# Load database file
	.self$.init.db()

	# Check that this field tag is defined in the fields list
	.self$isValidFieldTag(tag) || .self$message('error', paste0("Database field tag \"", tag, "\" is not valid."))

	# Check that columns are defined in database file
	all(colname %in% names(.self$.db)) || .self$message('error', paste0("One or more columns among ", paste(colname, collapse = ", "), " are not defined in database file."))

	# Set new definition
	if (length(colname) == 1)
		.self$.fields[[tag]] <- colname
	else {
		#new.col <- paste(colname, collapse = ".")
		if (tag %in% names(.self$.db))
			.self$message('error', paste("Column \"", tag, "\" already exist in database file.", sep = ''))
		.self$.db[[tag]] <- vapply(seq(nrow(.self$.db)), function(i) { paste(.self$.db[i, colname], collapse = '.') }, FUN.VALUE = '')
		.self$.fields[[tag]] <- tag
	}

	# Update data frame column names
	# XXX Why just update here and not init.db()?
#	colnames(.self$.db) <- vapply(.self$.db.orig.colnames, function(c) if (c %in% .self$.fields) names(.self$.fields)[.self$.fields %in% c] else c, FUN.VALUE = '')
})

# Set field multiple value separator {{{1
################################################################

MassCsvFileConn$methods( setFieldMultValSep = function(sep) {
	.field.multval.sep <<- sep
})

# Set ms modes {{{1
################################################################

MassCsvFileConn$methods( setMsMode = function(mode, value) {
	.self$.ms.modes[[mode]] <- value
})

# Check fields {{{1
################################################################

MassCsvFileConn$methods( .check.fields = function(fields, fail = TRUE) {

	if (length(fields) ==0 || (length(fields) == 1 && is.na(fields)))
		return

	# Check if fields are known
	unknown.fields <- names(.self$.fields)[ ! fields %in% names(.self$.fields)]
	if (length(unknown.fields) > 0)
		.self$message('error', paste0("Field(s) ", paste(fields, collapse = ", "), " is/are unknown."))

	# Init db
	.self$.init.db()

	# Check if fields are defined in file database
	undefined.fields <- colnames(.self$.db)[ ! .self$.fields[fields] %in% colnames(.self$.db)]
	if (length(undefined.fields) > 0) {
		.self$message(if (fail) 'error' else 'debug', paste0("Column(s) ", paste(.self$.fields[fields]), collapse = ", "), " is/are undefined in file database.")
		return(FALSE)
	}

	return(TRUE)
})

# Select {{{1
################################################################

# Select data from database
MassCsvFileConn$methods( .select = function(ids = NULL, cols = NULL, mode = NULL, compound.ids = NULL, drop = FALSE, uniq = FALSE, sort = FALSE, max.rows = NA_integer_, mz.min = NA_real_, mz.max = NA_real_, min.rel.int = NA_real_, precursor = FALSE, level = 0) {

	# Init db
	.self$.init.db()

	# Get db
	db <- .self$.db

	# Filter db on mode
	if ( ! is.null(mode) && ! is.na(mode)) {

		# Check mode value
		mode %in% names(.self$.ms.modes) || .self$message('error', paste0("Unknown mode value '", mode, "'."))
		.self$.check.fields('ms.mode')

		# Filter on mode
		db <- db[db[[.self$.fields[['ms.mode']]]] %in% .self$.ms.modes[[mode]], ]
	}

	# Filter db on ids
	if ( ! is.null(ids)) {
		.self$.check.fields('accession')
		db <- db[db[[.self$.fields[['accession']]]] %in% ids, ]
	}

	# Filter db on compound ids
	if ( ! is.null(compound.ids)) {
		.self$.check.fields('compound.id')
		db <- db[db[[.self$.fields[['compound.id']]]] %in% compound.ids, ]
	}

	# Filter on mz values
	if ((length(mz.min) > 0 || length(mz.max) > 0) && ! (length(mz.min) == 1 && is.na(mz.min) && is.na(mz.max))) {
		if (length(mz.min) != length(mz.max))
			.self$message(MSG.ERROR, paste("'mz.min' and 'mz.max' must have equal lengths. 'mz.min' has ", length(mz.min), " element(s), and 'mz.max' has ", length(mz.max), "element(s).", sep = ''))
		if (any(is.na(mz.min) & is.na(mz.max)))
		.self$.check.fields('peak.mztheo')
		f <- .self$.fields[['peak.mztheo']]
		mz <- db[[f]]
		s <- mapply(function(mzmin, mzmax) { (if (is.na(mzmin)) rep(TRUE, length(mz)) else mz >= mzmin) & (if (is.na(mzmax)) rep(TRUE, length(mz)) else  mz <= mzmax) }, mz.min, mz.max)
		s <- apply(s, 1, function(x) Reduce("&", x))
		db <- db[s, ]
	}

	# Filter on relative intensity
	if ( ! is.na(min.rel.int)) {
		if (.self$.check.fields('peak.relative.intensity', fail = false))
			db <- db[db[[.self$.fields[['peak.relative.intensity']]]] >= min.rel.int, ]
	}

	# Filter on precursors
	if (precursor) {
		if (.self$.check.fields('peak.attr', fail = false))
			db <- db[db[[.self$.fields[['peak.attr']]]] %in% .self$.precursors, ]
	}

	# Filter on MS level
	if (level > 0) {
		if (.self$.check.fields('ms.level', fail = false))
			db <- db[db[[.self$.fields[['ms.level']]]] == level, ]
	}

	# Get subset of columns
	if ( ! is.null(cols) && ! is.na(cols)) {
		.self$.check.fields(cols)
		db <- db[, .self$.fields[cols], drop = FALSE]
	}

	# Remove duplicates
	if (uniq)
		db <- db[ ! duplicated(db), , drop = FALSE]

	# Sort on first column
	if (sort && ncol(db) >= 1)
		db <- db[order(db[[1]]), , drop = FALSE]

	# Cut
	if ( ! is.na(max.rows) && nrow(db) > max.rows)
		db <- db[1:max.rows, , drop = FALSE]

	# Drop
	if (drop && ncol(db) == 1)
		db <- db[[1]]

	return(db)
})

# Get entry ids {{{1
################################################################

MassCsvFileConn$methods( getEntryIds = function(max.results = NA_integer_) {

	ids <- NA_character_

	ids <- as.character(.self$.select(cols =  'accession', drop = TRUE, uniq = TRUE, sort = TRUE, max.rows = max.results))

	return(ids)
})

# Get nb entries {{{1
################################################################

MassCsvFileConn$methods( getNbEntries = function(count = FALSE) {

	n <- NA_integer_

	ids <- .self$getEntryIds()
	if ( ! is.null(ids))
		n <- length(ids)

	return(n)
})

# Get chromatographic columns {{{1
################################################################

MassCsvFileConn$methods( getChromCol = function(compound.ids = NULL) {

	# Extract needed columns
	db <- .self$.select(cols = c('compound.id', 'chrom.col'), compound.ids = compound.ids)

	# Get column names
	cols <- db[[.self$.fields[['chrom.col']]]]

	# Remove NA values
	cols <- cols[ ! is.na(cols)]

	# Remove duplicates
	cols <- cols[ ! duplicated(cols)]

	# Make data frame
	if (is.null(cols))
		chrom.cols <- data.frame(a = character(0), b = character(0))
	else
		chrom.cols <- data.frame(cols, cols, stringsAsFactors = FALSE)
	colnames(chrom.cols) <- c('id', 'title')

	return(chrom.cols)
})

# Get mz values {{{1
################################################################

# Inherited from MassdbConn.
MassCsvFileConn$methods( .doGetMzValues = function(ms.mode, max.results, precursor, ms.level) {

	# Get mz values
	mz <- .self$.select(cols = 'peak.mztheo', mode = ms.mode, drop = TRUE, uniq = TRUE, sort = TRUE, max.rows = max.results, precursor = precursor, level = ms.level)

	return(mz)
})

# Get nb peaks {{{1
################################################################

# Inherited from MassdbConn.
MassCsvFileConn$methods( getNbPeaks = function(mode = NULL, compound.ids = NULL) {

	# Get peaks
	peaks <- .self$.select(cols = 'peak.mztheo', mode = mode, compound.ids = compound.ids, drop = TRUE)

	return(length(peaks))
})

# Get entry content {{{1
################################################################

MassCsvFileConn$methods( getEntryContent = function(entry.id) {

	# Initialize return values
	content <- rep(NA_character_, length(entry.id))

	# Get data frame
	.self$message('debug', paste("Entry entry.id:", paste(entry.id, collapse = ", ")))
	df <- .self$.select(ids = entry.id, uniq = TRUE, sort = TRUE)

	# For each id, take the sub data frame and convert it into string
	df.ids <- df[[.self$.fields[['accession']]]]
	content <- vapply(entry.id, function(x) if (is.na(x)) NA_character_ else { str.conn <- textConnection("str", "w", local = TRUE) ; write.table(df[df.ids == x, ], file = str.conn, row.names = FALSE, quote = FALSE, sep = "\t") ; close(str.conn) ; paste(str, collapse = "\n") }, FUN.VALUE = '')

	.self$message('debug', paste("Entry content:", content))

	return(content)
})

# Do search M/Z range {{{1
################################################################

MassCsvFileConn$methods( .doSearchMzRange = function(mz.min, mz.max, min.rel.int, ms.mode, max.results, precursor, ms.level) {
	return(.self$.select(mz.min = mz.min, mz.max = mz.max, min.rel.int = min.rel.int, mode = ms.mode, max.rows = max.results, cols = 'accession', drop = TRUE, uniq = TRUE, sort = TRUE, precursor = precursor, level = ms.level))
})
