# vi: fdm=marker

#' @include biodb-common.R
#' @include MassdbConn.R 

# In the provided file, each line represents an MS peak measure.
# The file contains molecule and spectrum information. Each spectrum has an accession id.

# Class declaration {{{1
################################################################

MassCsvFileConn <- methods::setRefClass("MassCsvFileConn", contains = "MassdbConn", fields = list(.file.sep = "character", .file.quote = "character", .field.multval.sep = 'character', .db = "ANY", .db.orig.colnames = "character", .fields = "character", .ms.modes = "character", .precursors = "character"))

# Constructor {{{1
################################################################

MassCsvFileConn$methods( initialize = function(file.sep = "\t", file.quote = "\"", ...) {

	callSuper(...)

	# Set fields
	.db <<- NULL
	.db.orig.colnames <<- NA_character_
	.file.sep <<- file.sep
	.file.quote <<- file.quote
	.fields <<- character()
	.field.multval.sep <<- ';'
	.ms.modes <<- BIODB.MSMODE.VALS
	names(.self$.ms.modes) <- BIODB.MSMODE.VALS

	# Precursors
	.precursors <<- c("[(M+H)]+", "[M+H]+", "[(M+Na)]+", "[M+Na]+", "[(M+K)]+", "[M+K]+", "[(M-H)]-", "[M-H]-", "[(M+Cl)]-", "[M+Cl]-")
})

# Get precursor formulae {{{1
################################################################

MassCsvFileConn$methods( getPrecursorFormulae = function() {
	":\n\nReturns the list of formulae used to recognize precursors."
	return (.self$.precursors)
})

# Is a precursor formula {{{1
################################################################

MassCsvFileConn$methods( isAPrecursorFormula = function(formula) {
	":\n\nReturns TRUE of the submitted formula is considered a precursor."
	return (formula %in% .self$.precursors)
})

# Add precursor formulae {{{1
################################################################

MassCsvFileConn$methods( addPrecursorFormulae = function(formulae) {
	":\n\nAdd new formulae to the list of formulae used to recognize precursors."
	if ( ! all(formulae %in% .self$.precursors)) {
		formulae <- formulae[ ! formulae %in% .self$.precursors]
		.precursors <<- c(.self$.precursors, formulae)
	}
})

# Is valid field tag {{{1
################################################################

MassCsvFileConn$methods( isValidFieldTag = function(tag) {
	return (tag %in% names(.self$.fields))
})

# Has field {{{1
################################################################

MassCsvFileConn$methods( hasField = function(tag) {

	tag <- tolower(tag)

	( ! is.null(tag) && ! is.na(tag)) || .self$message('error', "No tag specified.")

	# Load database file
	.self$.init.db()

	return(tag %in% names(.self$.fields))
})

# Add field {{{1
################################################################

MassCsvFileConn$methods( addField = function(tag, value) {
	":\n\nAdd a new field (column) to the database (data frame)."

	tag <- tolower(tag)

	( ! is.null(tag) && ! is.na(tag)) || .self$message('error', "No tag specified.")

	# Load database file
	.self$.init.db()

	# Field already defined?
	if (tag %in% names(.self$.fields))
		.self$message('error', paste0("Database field \"", tag, "\" is already defined."))
	if (tag %in% names(.self$.db))
		.self$message('error', paste0("Database column \"", tag, "\" is already defined."))

	# Add new field
	.self$message('debug', paste('Adding new field ', tag, ' with value ', paste(value, collapse = ', '), '.', sep = ''))
	.self$.db[[tag]] <- value
	.self$.fields[[tag]] <- tag
})

# Get field {{{1
################################################################

MassCsvFileConn$methods( getField = function(tag) {

	tag <- tolower(tag)

	( ! is.null(tag) && ! is.na(tag)) || .self$message('error', "No tag specified.")

	# Load database file
	.self$.init.db()

	# Check that this field tag is defined in the fields list
	if ( ! tag %in% names(.self$.fields))
		.self$message('error', paste0("Database field tag \"", tag, "\" is not defined."))

	return(.self$.fields[[tag]])
})

# Set field {{{1
################################################################

MassCsvFileConn$methods( setField = function(tag, colname, ignore.if.missing = FALSE) {

	tag <- tolower(tag)

	( ! is.null(tag) && ! is.na(tag)) || .self$message('error', "No tag specified.")
	( ! is.null(colname) && ! is.na(colname)) || .self$message('error', "No column name specified.")

	# Load database file
	.self$.init.db()

	# Check that this is a correct field name
	if ( ! (.self$getBiodb()$getEntryFields()$isDefined(tag) || tag %in% BIODB.PEAK.FIELDS)) {
		if ( ! ignore.if.missing)
			.self$message('error', paste0("Database field \"", tag, "\" is not valid."))
		return()
	}

	# Fail if column names are not found in file
	if ( ! all(colname %in% names(.self$.db))) {
		undefined.cols <- colname[ ! colname %in% names(.self$.db)]
		.self$message((if (ignore.if.missing) 'caution' else 'error'), paste0("Column(s) ", paste(undefined.cols, collapse = ", "), " is/are not defined in database file."))
		return()
	}

	.self$message('debug', paste('Set field ', tag, ' to column(s) ', paste(colname, collapse = ', '), '.', sep = ''))
	              
	# One column used, only
	if (length(colname) == 1) {
		.self$.fields[[tag]] <- colname
	}

	# Use several column to join together
	else {
		.self$.db[[tag]] <- vapply(seq(nrow(.self$.db)), function(i) { paste(.self$.db[i, colname], collapse = '.') }, FUN.VALUE = '')
		.self$.fields[[tag]] <- tag
	}
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

MassCsvFileConn$methods( getChromCol = function(ids = NULL) {

	# Extract needed columns
	db <- .self$.select(cols = 'chrom.col.name', ids = ids)

	# Get column names
	cols <- db[[.self$.fields[['chrom.col.name']]]]

	# Remove NA values
	cols <- cols[ ! is.na(cols)]

	# Remove duplicates
	cols <- cols[ ! duplicated(cols)]

	# Make data frame
	if (is.null(cols))
		chrom.cols <- data.frame(a = character(0), b = character(0))
	else
		chrom.cols <- data.frame(cols, cols, stringsAsFactors = FALSE)
	names(chrom.cols) <- c('id', 'title')

	return(chrom.cols)
})

# Get nb peaks {{{1
################################################################

# Inherited from MassdbConn.
MassCsvFileConn$methods( getNbPeaks = function(mode = NULL, ids = NULL) {

	# Get peaks
	peaks <- .self$.select(cols = 'peak.mztheo', mode = mode, ids = ids, drop = TRUE)

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

# Set database {{{1
################################################################

MassCsvFileConn$methods( setDb = function(db) {
	":\n\nSet the database directly from a data frame. You must not have set the database previously with the URL parameter."

	# Already set
	if ( ! is.null(.self$.db))
		.self$message('error', 'Database has already been set.')

	# Not data frame
	if ( ! is.data.frame(db))
		.self$message('error', 'The database object must be a data frame.')

	# Set data frame as database
	.db <<- db

	# Set fields
	for (field in names(.self$.db))
		.self$setField(field, field, ignore.if.missing = TRUE)

	# Save column names
	.db.orig.colnames <<- colnames(.self$.db)
})

# PRIVATE METHODS {{{1
################################################################

# Init db {{{2
################################################################

MassCsvFileConn$methods( .init.db = function() {

	if (is.null(.self$.db)) {

		# Check file
		if ( ! file.exists(.self$getBaseUrl()))
			.self$message('error', paste("Cannot locate the file database \"", .self$getBaseUrl() ,"\".", sep = ''))

		# Load database
		db <- read.table(.self$getBaseUrl(), sep = .self$.file.sep, quote = .self$.file.quote, header = TRUE, stringsAsFactors = FALSE, row.names = NULL, comment.char = '', check.names = FALSE, fill = FALSE)

		# Set database
		.self$setDb(db)
	}
})

# Check fields {{{2
################################################################

MassCsvFileConn$methods( .check.fields = function(fields, fail = TRUE) {

	if (length(fields) == 0 || (length(fields) == 1 && is.na(fields)))
		return

	# Check if fields are known
	unknown.fields <- fields[ ! vapply(fields, function(f) .self$getBiodb()$getEntryFields()$isDefined(f), FUN.VALUE = FALSE)]
	if (length(unknown.fields) > 0)
		.self$message('error', paste0("Field(s) ", paste(fields, collapse = ", "), " is/are unknown."))

	# Init db
	.self$.init.db()

	# Check if fields are defined in file database
	undefined.fields <- fields[ ! fields %in% names(.self$.fields)]
	if (length(undefined.fields) > 0) {
		.self$message((if (fail) 'error' else 'debug'), paste0("Field(s) ", paste(undefined.fields, collapse = ", "), " is/are undefined in file database."))
		return(FALSE)
	}

	return(TRUE)
})

# Select {{{2
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
		.self$message('debug', paste('Filtering on M/Z range [', mz.min, ', ', mz.max, '].', sep = ''))
		if (length(mz.min) != length(mz.max))
			.self$message(MSG.ERROR, paste("'mz.min' and 'mz.max' must have equal lengths. 'mz.min' has ", length(mz.min), " element(s), and 'mz.max' has ", length(mz.max), "element(s).", sep = ''))
		if (any(is.na(mz.min) & is.na(mz.max)))
		.self$.check.fields('peak.mztheo')
		f <- .self$.fields[['peak.mztheo']]
		mz <- db[[f]]
		.self$message('debug', paste(length(mz), 'M/Z values to filter.'))

		# For all couples in vectors mz.min and mz.max, verify which M/Z values in mz are in the range. For each couple of mz.min/mz.max we get a vector of booleans the same length as mz.
		.self$message('debug', paste('mz.min = ', paste(mz.min, collapse = ', '), '.', sep = ''))
		.self$message('debug', paste('mz.max = ', paste(mz.max, collapse = ', '), '.', sep = ''))
		s <- mapply(function(mzmin, mzmax) { (if (is.na(mzmin)) rep(TRUE, length(mz)) else mz >= mzmin) & (if (is.na(mzmax)) rep(TRUE, length(mz)) else  mz <= mzmax) }, mz.min, mz.max)

		# Now we select the M/Z values that are in all ranges listed in mz.min/mz.max.
		if (is.matrix(s))
			s <- apply(s, 1, function(x) Reduce("&", x))

		db <- db[s, ]
	}

	# Filter on relative intensity
	if ( ! is.na(min.rel.int)) {
		if (.self$.check.fields('peak.relative.intensity', fail = FALSE))
			db <- db[db[[.self$.fields[['peak.relative.intensity']]]] >= min.rel.int, ]
		else
			db <- db[integer(),]
	}

	# Filter on precursors
	if (precursor) {
		if (.self$.check.fields('peak.attr', fail = FALSE))
			db <- db[db[[.self$.fields[['peak.attr']]]] %in% .self$.precursors, ]
		else
			db <- db[integer(),]
	}

	# Filter on MS level
	if (level > 0) {
		if (.self$.check.fields('ms.level', fail = FALSE))
			db <- db[db[[.self$.fields[['ms.level']]]] == level, ]
		else
			db <- db[integer(),]
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
	if ( ! is.na(max.rows) && max.rows > 0 && nrow(db) > max.rows)
		db <- db[1:max.rows, , drop = FALSE]

	# Drop
	if (drop && ncol(db) == 1)
		db <- db[[1]]

	return(db)
})

# Do search M/Z range {{{2
################################################################

MassCsvFileConn$methods( .doSearchMzRange = function(mz.min, mz.max, min.rel.int, ms.mode, max.results, precursor, ms.level) {
	return(.self$.select(mz.min = mz.min, mz.max = mz.max, min.rel.int = min.rel.int, mode = ms.mode, max.rows = max.results, cols = 'accession', drop = TRUE, uniq = TRUE, sort = TRUE, precursor = precursor, level = ms.level))
})

# Do get mz values {{{2
################################################################

# Inherited from MassdbConn.
MassCsvFileConn$methods( .doGetMzValues = function(ms.mode, max.results, precursor, ms.level) {

	# Get mz values
	mz <- .self$.select(cols = 'peak.mztheo', mode = ms.mode, drop = TRUE, uniq = TRUE, sort = TRUE, max.rows = max.results, precursor = precursor, level = ms.level)

	return(mz)
})
