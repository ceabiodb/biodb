# vi: fdm=marker

#' @include biodb-common.R
#' @include MassdbConn.R 
#' @include BiodbWritable.R 

# In the provided file, each line represents an MS peak measure.
# The file contains molecule and spectrum information. Each spectrum has an accession id.

# Class declaration {{{1
################################################################

MassCsvFileConn <- methods::setRefClass("MassCsvFileConn", contains = c("MassdbConn", 'BiodbWritable', 'BiodbEditable'), fields = list(.file.sep = "character", .file.quote = "character", .field.multval.sep = 'character', .db = "ANY", .db.orig.colnames = "character", .fields = "character", .precursors = "character", .parsing.expr = 'list'))

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
	.parsing.expr <<- list()

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

# Set precursor formulae {{{1
################################################################

MassCsvFileConn$methods( setPrecursorFormulae = function(formulae) {
	":\n\nReplace current formulae by this new list of formulae."
	.self$.assert.is(formulae, 'character')
	.precursors <<- formulae[ ! duplicated(formulae)]
})

# Add precursor formulae {{{1
################################################################

MassCsvFileConn$methods( addPrecursorFormulae = function(formulae) {
	":\n\nAdd new formulae to the list of formulae used to recognize precursors."

	.self$.check.parsing.has.began()

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

	.self$.check.parsing.has.began()

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

	.self$.check.parsing.has.began()

	tag <- tolower(tag)

	( ! is.null(tag) && ! is.na(tag)) || .self$message('error', "No tag specified.")
	( ! is.null(colname) && ! is.na(colname)) || .self$message('error', "No column name specified.")

	# Load database file
	.self$.init.db()

	# Check that this is a correct field name
	if ( ! .self$getBiodb()$getEntryFields()$isDefined(tag)) {
		if ( ! ignore.if.missing)
			.self$message('error', paste0("Database field \"", tag, "\" is not valid."))
		return()
	}

	# Set real name (i.e.: official name) for field
	tag <- .self$getBiodb()$getEntryFields()$getRealName(tag)

	# Fail if column names are not found in file
	if ( ! all(colname %in% names(.self$.db))) {
		undefined.cols <- colname[ ! colname %in% names(.self$.db)]
		.self$message((if (ignore.if.missing) 'caution' else 'error'), paste0("Column(s) ", paste(undefined.cols, collapse = ", "), " is/are not defined in database file."))
		return()
	}

	.self$message('debug', paste('Set field ', tag, ' to column(s) ', paste(colname, collapse = ', '), '.', sep = ''))
	              
	# One column used, only
	if (length(colname) == 1) {

		# Check values
		if (.self$getBiodb()$getEntryFields()$isDefined(tag)) {
			entry.field <- .self$getBiodb()$getEntryFields()$get(tag)

			# Check values of enumerate type
			if (entry.field$isEnumerate()) {
				entry.field$checkValue(.self$.db[[colname]])
				.self$.db[[colname]] <- entry.field$correctValue(.self$.db[[colname]])
			}
		}


		# Set field
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

	.self$.check.parsing.has.began()

	.field.multval.sep <<- sep
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
	content <- vapply(entry.id, function(x) { if (is.na(x)) NA_character_ else { x.df <- .self$.select(ids = x) ; if (nrow(x.df) == 0) NA_character_ else { str.conn <- textConnection("str", "w", local = TRUE) ; write.table(x.df, file = str.conn, row.names = FALSE, quote = FALSE, sep = "\t") ; close(str.conn) ; paste(str, collapse = "\n") } } }, FUN.VALUE = '')

	if (length(content) > 0)
		.self$message('debug', paste("Content of first entry:", content[[1]]))

	return(content)
})

# Set database {{{1
################################################################

MassCsvFileConn$methods( setDb = function(db) {
	":\n\nSet the database directly from a data frame. You must not have set the database previously with the URL parameter."

	# URL point to an existing file?
	url <- .self$getBaseUrl()
	if ( ! is.null(url) && ! is.na(url) && file.exists(url))
		.self$message('error', 'Cannot set this data frame as database. A URL that points to an existing file has already been set for the connector.')

	.self$.doSetDb(db)
})

# Private methods {{{1
################################################################

# Writable methods {{{2
################################################################

# Do write {{{3
################################################################

MassCsvFileConn$methods( .doWrite = function() {

	# Make sure all entries are loaded into cache.
	entry.ids <- .self$getEntryIds()
	entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), entry.ids)

	# Get all entries: the ones loaded from the database file and the ones created in memory (and not saved).
	entries <- .self$getAllCacheEntries()

	# Get data frame of all entries
	df <- .self$getBiodb()$entriesToDataframe(entries, only.atomic = FALSE)

	# Write data frame
	write.table(df, file = .self$getBaseUrl(), row.names = FALSE, sep = "\t", quote = FALSE)
})

# Init db {{{2
################################################################

MassCsvFileConn$methods( .init.db = function() {

	if (is.null(.self$.db)) {

		# Check file
		file <- .self$getBaseUrl()
		if ( ! is.null(file) && ! is.na(file) && ! file.exists(file))
			.self$message('info', paste("Cannot locate the file database \"", file, "\".", sep = ''))

		# No file to load
		if (is.null(file) || is.na(file) || ! file.exists(file)) {
			.self$message('info', "Creating empty database.")
			db <- data.frame(accession = character(), stringsAsFactors = FALSE)
		}

		# Load database
		else {
			.self$message('info', paste("Loading file database \"", file, "\".", sep = ''))
			db <- read.table(.self$getBaseUrl(), sep = .self$.file.sep, quote = .self$.file.quote, header = TRUE, stringsAsFactors = FALSE, row.names = NULL, comment.char = '', check.names = FALSE, fill = FALSE)
		}

		# Set database
		.self$.doSetDb(db)
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
MassCsvFileConn$methods( .select = function(ids = NULL, cols = NULL, mode = NULL, compound.ids = NULL, drop = FALSE, uniq = FALSE, sort = FALSE, max.rows = NA_integer_, mz.min = NULL, mz.max = NULL, min.rel.int = NA_real_, precursor = FALSE, level = 0) {

	# Init db
	.self$.init.db()

	# Get db
	db <- .self$.db

	# Filter db on mode
	if ( ! is.null(mode) && ! is.na(mode)) {

		# Check mode value
		ms.mode.field <- .self$getBiodb()$getEntryFields()$get('ms.mode')
		ms.mode.field$checkValue(mode)
		.self$.check.fields('ms.mode')

		# Filter on mode
		db <- db[db[[.self$.fields[['ms.mode']]]] %in% ms.mode.field$getAllowedValues(mode), , drop = FALSE]
	}

	# Filter db on ids
	if ( ! is.null(ids)) {
		.self$.check.fields('accession')
		db <- db[db[[.self$.fields[['accession']]]] %in% ids, , drop = FALSE]
	}

	# Filter db on compound ids
	if ( ! is.null(compound.ids)) {
		.self$.check.fields('compound.id')
		db <- db[db[[.self$.fields[['compound.id']]]] %in% compound.ids, , drop = FALSE]
	}

	# Filter on mz values
	if ( ! is.null(mz.min) || ! is.null(mz.max)) {
		if (is.null(mz.min) || is.null(mz.max))
			.self$message('error', 'You must set both mz.min and mz.max.')
		if (length(mz.min) != length(mz.max))
			.self$message('error', paste("'mz.min' and 'mz.max' must have equal lengths. 'mz.min' has ", length(mz.min), " element(s), and 'mz.max' has ", length(mz.max), "element(s).", sep = ''))
		
		.self$message('debug', paste0('Filtering on M/Z ranges: ', paste0('[', mz.min, ', ', mz.max, ']', collapse = ', '), '.'))
		.self$.check.fields('peak.mztheo')
		f <- .self$.fields[['peak.mztheo']]
		mz <- db[[f]]
		.self$message('debug', paste(length(mz), 'M/Z values to filter.'))

		# For all couples in vectors mz.min and mz.max, verify which M/Z values in mz are in the range. For each couple of mz.min/mz.max we get a vector of booleans the same length as mz.
		s <- mapply(function(mzmin, mzmax) { if (is.na(mzmin) && is.na(mzmax)) rep(FALSE, length(mz)) else ((if (is.na(mzmin)) rep(TRUE, length(mz)) else mz >= mzmin) & (if (is.na(mzmax)) rep(TRUE, length(mz)) else  mz <= mzmax)) }, mz.min, mz.max)

		# Now we select the M/Z values that are in at least one of the M/Z ranges.
		if (is.matrix(s))
			s <- apply(s, 1, function(x) Reduce("|", x))
		else if (is.list(s))
			s <- unlist(s)

		db <- db[s, , drop = FALSE]
	}

	# Filter on relative intensity
	if ( ! is.na(min.rel.int)) {
		if (.self$.check.fields('peak.relative.intensity', fail = FALSE))
			db <- db[db[[.self$.fields[['peak.relative.intensity']]]] >= min.rel.int, , drop = FALSE]
		else
			db <- db[integer(), , drop = FALSE]
	}

	# Filter on precursors
	if (precursor) {
		if (.self$.check.fields('peak.attr', fail = FALSE))
			db <- db[db[[.self$.fields[['peak.attr']]]] %in% .self$.precursors, , drop = FALSE]
		else
			db <- db[integer(), , drop = FALSE]
	}

	# Filter on MS level
	if (level > 0) {
		if (.self$.check.fields('ms.level', fail = FALSE))
			db <- db[db[[.self$.fields[['ms.level']]]] == level, , drop = FALSE]
		else
			db <- db[integer(), , drop = FALSE]
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

# Do set database data frame {{{2
################################################################

MassCsvFileConn$methods( .doSetDb = function(db) {

	# Already set?
	if ( ! is.null(.self$.db))
		.self$message('error', 'Database has already been set.')

	# Not a data frame
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

# Check setting of URL {{{2
################################################################

MassCsvFileConn$methods( .checkSettingOfUrl = function(key, value) {

	# Setting of base URL
	if (key == 'base.url') {
		url <- .self$getBaseUrl()
		if ( ! is.null(.self$.db) && ! is.null(url) && ! is.na(url) && file.exists(url))
			.self$message('error', paste0('You cannot overwrite base URL. A URL has already been set ("', url, '") that points to a valid file that has already been loaded in memory.'))
	}
})

# Get parsing expressions {{{2
################################################################

MassCsvFileConn$methods( .getParsingExpressions = function() {

	if (length(.self$.parsing.expr) == 0) {
		.parsing.expr <<- list()
		entry.fields <- .self$getBiodb()$getEntryFields()

		# Loop on all fields defined in database
		for (field in names(.self$.fields)) {
			f <- entry.fields$get(field)
			if (is.null(f) || is.na(f$getGroup()) || f$getGroup() != 'peak')
				.self$.parsing.expr[[field]] <- .self$.fields[[field]]
		}

		# Loop on all entry fields
		for (field in entry.fields$getFieldNames())
			if ( ! field %in% names(.self$.fields)) {
			f <- entry.fields$get(field)
			if (is.na(f$getGroup()) || f$getGroup() != 'peak')
				.self$.parsing.expr[[field]] <- field
		}

	}

	return(.self$.parsing.expr)
})

# Check if parsing has began {{{2
################################################################

MassCsvFileConn$methods( .check.parsing.has.began = function() {

	# Parsing has began?
	if (length(.self$.parsing.expr) > 0)
		.self$message('error', 'Action impossible, parsing of entries has already began.')
})
