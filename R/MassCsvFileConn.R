# vi: fdm=marker

#' @include biodb-common.R
#' @include MassdbConn.R 

# In the provided file, each line represents an MS peak measure.
# The file contains molecule and spectrum information. Each spectrum has an accession id.

# Constants {{{1
################################################################

# Default database fields
.BIODB.DFT.DB.FIELDS <- c(BIODB.ACCESSION, BIODB.NAME, BIODB.FULLNAMES, BIODB.COMPOUND.ID, BIODB.MS.LEVEL, BIODB.MSMODE, BIODB.PEAK.MZEXP, BIODB.PEAK.MZTHEO, BIODB.PEAK.RELATIVE.INTENSITY, BIODB.PEAK.COMP, BIODB.PEAK.ATTR, BIODB.CHROM.COL, BIODB.CHROM.COL.RT, BIODB.FORMULA, BIODB.MASS, BIODB.INCHI, BIODB.INCHIKEY, BIODB.CHEBI.ID, BIODB.HMDB.METABOLITE.ID, BIODB.KEGG.COMPOUND.ID, BIODB.NCBI.PUBCHEM.COMP.ID)
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
		.self$message(MSG.ERROR, paste("Cannot locate the file database \"", .self$getBaseUrl() ,"\".", sep = ''))

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

	( ! is.null(tag) && ! is.na(tag)) || .self$message(MSG.ERROR, "No tag specified.")
	( ! is.null(colname) && ! is.na(colname)) || .self$message(MSG.ERROR, "No column name specified.")

	# Load database file
	.self$.init.db()

	# Check that this field tag is defined in the fields list
	.self$isValidFieldTag(tag) || .self$message(MSG.ERROR, paste0("Database field tag \"", tag, "\" is not valid."))

	# Check that columns are defined in database file
	all(colname %in% names(.self$.db)) || .self$message(MSG.ERROR, paste0("One or more columns among ", paste(colname, collapse = ", "), " are not defined in database file."))

	# Set new definition
	if (length(colname) == 1)
		.self$.fields[[tag]] <- colname
	else {
		new.col <- paste(colname, collapse = ".")
		.self$.db[[new.col]] <- vapply(seq(nrow(.self$.db)), function(i) { paste(.self$.db[i, colname], collapse = '.') }, FUN.VALUE = '')
		.self$.fields[[tag]] <- new.col
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
		.self$message(MSG.ERROR, paste0("Field(s) ", paste(fields, collapse = ", "), " is/are unknown."))

	# Init db
	.self$.init.db()

	# Check if fields are defined in file database
	undefined.fields <- colnames(.self$.db)[ ! .self$.fields[fields] %in% colnames(.self$.db)]
	if (length(undefined.fields) > 0) {
		.self$message(if (fail) MSG.ERROR else MSG.DEBUG, paste0("Column(s) ", paste(.self$.fields[fields]), collapse = ", "), " is/are undefined in file database.")
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
		mode %in% names(.self$.ms.modes) || .self$message(MSG.ERROR, paste0("Unknown mode value '", mode, "'."))
		.self$.check.fields(BIODB.MSMODE)

		# Filter on mode
		db <- db[db[[.self$.fields[[BIODB.MSMODE]]]] %in% .self$.ms.modes[[mode]], ]
	}

	# Filter db on ids
	if ( ! is.null(ids)) {
		.self$.check.fields(BIODB.ACCESSION)
		db <- db[db[[.self$.fields[[BIODB.ACCESSION]]]] %in% ids, ]
	}

	# Filter db on compound ids
	if ( ! is.null(compound.ids)) {
		.self$.check.fields(BIODB.COMPOUND.ID)
		db <- db[db[[.self$.fields[[BIODB.COMPOUND.ID]]]] %in% compound.ids, ]
	}

	# Filter on mz values
	if ( ! is.na(mz.min)) {
		.self$.check.fields(BIODB.PEAK.MZTHEO)
		db <- db[db[[.self$.fields[[BIODB.PEAK.MZTHEO]]]] >= mz.min, ]
	}
	if ( ! is.na(mz.max)) {
		.self$.check.fields(BIODB.PEAK.MZTHEO)
		db <- db[db[[.self$.fields[[BIODB.PEAK.MZTHEO]]]] <= mz.max, ]
	}

	# Filter on relative intensity
	if ( ! is.na(min.rel.int)) {
		if (.self$.check.fields(BIODB.PEAK.RELATIVE.INTENSITY, fail = FALSE))
			db <- db[db[[.self$.fields[[BIODB.PEAK.RELATIVE.INTENSITY]]]] >= mz.min, ]
	}

	# Filter on precursors
	if (precursor) {
		if (.self$.check.fields(BIODB.PEAK.ATTR, fail = FALSE))
			db <- db[db[[.self$.fields[[BIODB.PEAK.ATTR]]]] %in% .self$.precursors, ]
	}

	# Filter on MS level
	if (level > 0) {
		if (.self$.check.fields(BIODB.MS.LEVEL, fail = FALSE))
			db <- db[db[[.self$.fields[[BIODB.MS.LEVEL]]]] == level, ]
	}

	# Get subset of columns
	if ( ! is.null(cols) && ! is.na(cols)) {
		.self$.check.fields(cols)
		db <- db[, .self$.fields[cols], drop = drop]
	}

	# Remove duplicates
	if (uniq) {
		if (is.vector(db))
			db <- db[ ! duplicated(db)]
		else
			db <- db[ ! duplicated(db), ]
	}

	# Sort
	if (sort && is.vector(db))
		db <- sort(db)

	# Cut
	if ( ! is.na(max.rows))
		db <- if (is.vector(db)) db[1:max.rows] else db[1:max.rows, ]

	return(db)
})

# Get entry ids {{{1
################################################################

MassCsvFileConn$methods( getEntryIds = function(max.results = NA_integer_) {

	ids <- NA_character_

	ids <- as.character(.self$.select(cols =  BIODB.ACCESSION, drop = TRUE, uniq = TRUE, sort = TRUE, max.rows = max.results))

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
	db <- .self$.select(cols = c(BIODB.COMPOUND.ID, BIODB.CHROM.COL), compound.ids = compound.ids)

	# Get column names
	cols <- db[[.self$.fields[[BIODB.CHROM.COL]]]]

	# Remove NA values
	cols <- cols[ ! is.na(cols)]

	# Remove duplicates
	cols <- cols[ ! duplicated(cols)]

	# Make data frame
	if (is.null(cols))
		chrom.cols <- data.frame(a = character(0), b = character(0))
	else
		chrom.cols <- data.frame(cols, cols, stringsAsFactors = FALSE)
	colnames(chrom.cols) <- c(BIODB.ID, BIODB.TITLE)

	return(chrom.cols)
})

# Get mz values {{{1
################################################################

# Inherited from MassdbConn.
MassCsvFileConn$methods( .doGetMzValues = function(ms.mode, max.results, precursor, level) {

	# Get mz values
	mz <- .self$.select(cols = BIODB.PEAK.MZTHEO, mode = ms.mode, drop = TRUE, uniq = TRUE, sort = TRUE, max.rows = max.results, precursor = precursor, level = level)

	return(mz)
})

# Get nb peaks {{{1
################################################################

# Inherited from MassdbConn.
MassCsvFileConn$methods( getNbPeaks = function(mode = NULL, compound.ids = NULL) {

	# Get peaks
	peaks <- .self$.select(cols = BIODB.PEAK.MZTHEO, mode = mode, compound.ids = compound.ids, drop = TRUE)

	return(length(peaks))
})

# Get entry content {{{1
################################################################

MassCsvFileConn$methods( getEntryContent = function(id) {

	# Initialize return values
	content <- rep(NA_character_, length(id))

	# Get data frame
	.self$message(MSG.DEBUG, paste("Entry id:", paste(id, collapse = ", ")))
	df <- .self$.select(ids = id, uniq = TRUE, sort = TRUE)

	# For each id, take the sub data frame and convert it into string
	df.ids <- df[[.self$.fields[[BIODB.ACCESSION]]]]
	content <- vapply(id, function(x) if (is.na(x)) NA_character_ else { str.conn <- textConnection("str", "w", local = TRUE) ; write.table(df[df.ids == x, ], file = str.conn, row.names = FALSE, quote = FALSE, sep = "\t") ; close(str.conn) ; paste(str, collapse = "\n") }, FUN.VALUE = '')

	.self$message(MSG.DEBUG, paste("Entry content:", content))

	return(content)
})

# Do search M/Z range {{{1
################################################################

MassCsvFileConn$methods( .doSearchMzRange = function(mz.min, mz.max, min.rel.int, ms.mode, max.results, precursor, ms.level) {
	return(.self$.select(mz.min = mz.min, mz.max = mz.max, min.rel.int = min.rel.int, mode = ms.mode, max.rows = max.results, cols = BIODB.ACCESSION, drop = TRUE, uniq = TRUE, sort = TRUE, precursor = precursor, level = ms.level))
})
