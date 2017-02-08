# vi: fdm=marker

# In the provided file, each line represents an MS peak measure.
# The file contains molecule and spectrum information. Each spectrum has an accession id.

# Constants {{{1
################################################################

# Default database fields
.BIODB.DFT.DB.FIELDS <- c(BIODB.ACCESSION, BIODB.NAME, BIODB.FULLNAMES, BIODB.COMPOUND.ID, BIODB.MSMODE, BIODB.PEAK.MZEXP, BIODB.PEAK.MZTHEO, BIODB.PEAK.COMP, BIODB.PEAK.ATTR, BIODB.CHROM.COL, BIODB.CHROM.COL.RT, BIODB.FORMULA, BIODB.MASS)
names(.BIODB.DFT.DB.FIELDS) <- .BIODB.DFT.DB.FIELDS

# Class declaration {{{1
################################################################

MassFiledbConn <- methods::setRefClass("MassFiledbConn", contains = "MassdbConn", fields = list(.file = "character", .file.sep = "character", .file.quote = "character", .field.multval.sep = 'character', .db = "ANY", .db.orig.colnames = "character", .fields = "character", .ms.modes = "character"))

# Constructor {{{1
################################################################

MassFiledbConn$methods( initialize = function(file = NA_character_, file.sep = "\t", file.quote = "\"", ...) {

	# Check file
	if (is.null(file) || is.na(file))
		.self$message(MSG.ERROR, "You must specify a file database to load.")
	if ( ! file.exists(file))
		.self$message(MSG.ERROR, paste("Cannot locate the file database \"", file ,"\".", sep = ''))

	# Set fields
	.db <<- NULL
	.db.orig.colnames <<- NA_character_
	.file <<- file
	.file.sep <<- file.sep
	.file.quote <<- file.quote
	.fields <<- .BIODB.DFT.DB.FIELDS
	.field.multval.sep <<- ';'
	.ms.modes <<- c(BIODB.MSMODE.NEG, BIODB.MSMODE.POS)
	names(.self$.ms.modes) <- .self$.ms.modes

	callSuper(...)
})

# Is valid field tag {{{1
################################################################

MassFiledbConn$methods( isValidFieldTag = function(tag) {
	return (tag %in% names(.self$.fields))
})

# Init db {{{1
################################################################

MassFiledbConn$methods( .init.db = function() {

	if (is.null(.self$.db)) {

		# Load database
		.db <<- read.table(.self$.file, sep = .self$.file.sep, .self$.file.quote, header = TRUE, stringsAsFactors = FALSE, row.names = NULL)

		# Save column names
		.db.orig.colnames <<- colnames(.self$.db)
	}
})

# Set field {{{1
################################################################

MassFiledbConn$methods( setField = function(tag, colname) {

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

MassFiledbConn$methods( setFieldMultValSep = function(sep) {
	.field.multval.sep <<- sep
})

# Set ms modes {{{1
################################################################

MassFiledbConn$methods( setMsMode = function(mode, value) {
	.self$.ms.modes[[mode]] <- value
})

# Get entry content type {{{1
################################################################

MassFiledbConn$methods( getEntryContentType = function() {
	return(BIODB.TSV)
})

# Check fields {{{1
################################################################

MassFiledbConn$methods( .check.fields = function(fields) {

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
	if (length(undefined.fields) > 0)
		.self$message(MSG.ERROR, paste0("Column(s) ", paste(.self$.fields[fields]), collapse = ", "), " is/are undefined in file database.")
})

# Select {{{1
################################################################

# Select data from database
MassFiledbConn$methods( .select = function(ids = NULL, cols = NULL, mode = NULL, compound.ids = NULL, drop = FALSE, uniq = FALSE, sort = FALSE, max.rows = NA_integer_) {

	x <- NULL

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
		print('================================================================')
		print('Select on IDS:')
		print(ids)
		print('================================================================')
		.self$.check.fields(BIODB.ACCESSION)
		db <- db[db[[.self$.fields[[BIODB.ACCESSION]]]] %in% ids, ]
		print(db)
		print('================================================================')
	}

	# Filter db on compound ids
	if ( ! is.null(compound.ids)) {
		.self$.check.fields(BIODB.COMPOUND.ID)
		db <- db[db[[.self$.fields[[BIODB.COMPOUND.ID]]]] %in% compound.ids, ]
	}

	if ( ! is.null(cols) && ! is.na(cols))
		.self$.check.fields(cols)

	# Get subset
	if (is.null(cols) || is.na(cols))
		x <- db
	else
		x <- db[, .self$.fields[cols], drop = drop]

	# Rearrange
	if (drop && is.vector(x)) {
		if (uniq)
			x <- x[ ! duplicated(x)]
		if (sort)
			x <- sort(x)
	}

	# Cut
	if ( ! is.na(max.rows))
		x <- if (is.vector(x)) x[1:max.rows] else x[1:max.rows, ]

	return(x)
})

# Get entry ids {{{1
################################################################

MassFiledbConn$methods( getEntryIds = function(max.results = NA_integer_) {

	ids <- NA_character_

	ids <- as.character(.self$.select(cols =  BIODB.ACCESSION, drop = TRUE, uniq = TRUE, sort = TRUE))

	return(ids)
})

# Get chromatographic columns {{{1
################################################################

# Inherited from MassdbConn.
MassFiledbConn$methods( getChromCol = function(compound.ids = NULL) {

	# Extract needed columns
	db <- .self$.select(cols = c(BIODB.COMPOUND.ID, BIODB.CHROM.COL))

	# Filter on molecule IDs
	if ( ! is.null(compound.ids))
		db <- db[db[[BIODB.COMPOUND.ID]] %in% compound.ids, ]

	# Get column names
	cols <- db[[BIODB.CHROM.COL]]

	# Remove duplicates
	cols <- cols[ ! duplicated(cols)]

	# Make data frame
	chrom.cols <- data.frame(cols, cols, stringsAsFactors = FALSE)
	colnames(chrom.cols) <- c(BIODB.ID, BIODB.TITLE)

	return(chrom.cols)
})

# Get mz values {{{1
################################################################

# Inherited from MassdbConn.
MassFiledbConn$methods( getMzValues = function(mode = NULL, max.results = NA_integer_) {

	# Get mz values
	mz <- .self$.select(cols = BIODB.PEAK.MZ, mode = mode, drop = TRUE, uniq = TRUE, sort = TRUE, max.rows = max.results)

	return(mz)
})

# Get nb peaks {{{1
################################################################

# Inherited from MassdbConn.
MassFiledbConn$methods( getNbPeaks = function(mode = NULL, compound.ids = NULL) {

	# Get peaks
	peaks <- .self$.select(cols = BIODB.PEAK.MZTHEO, mode = mode, compound.ids = compound.ids)

	return(length(peaks))
})

# Get entry content {{{1
################################################################

MassFiledbConn$methods( getEntryContent = function(id) {

	# Initialize return values
	content <- rep(NA_character_, length(id))

	# Get data frame
	.self$message(MSG.DEBUG, paste("Entry id:", paste(id, collapse = ", ")))
	df <- .self$.select(ids = id, uniq = TRUE, sort = TRUE)
	print('****************************************************************')
	print(id)
	print('****************************************************************')
	print(.self$.db[1:2,])
	print('****************************************************************')
	print(df)
	print('****************************************************************')

	# For each id, take the sub data frame and convert it into string
	content <- vapply(id, function(x) if (is.na(x)) NA_character_ else { c <- '' ; write.table(df[df[[BIODB.ACCESSION]] == id, ], textConnection("c", "w", local = TRUE), row.names = FALSE, quote = FALSE, sep = "\t") ; c }, FUN.VALUE = '')

	.self$message(MSG.DEBUG, paste("Entry content:", content))

	return(content)
})

# Create entry {{{1
################################################################

MassFiledbConn$methods( createEntry = function(content, drop = TRUE) {

	entries <- list()

	# Loop on all contents
	for (single.content in content) {

		# Create instance
		entry <- BiodbEntry$new(.self$getBiodb())

		if ( ! is.null(single.content) && ! is.na(single.content)) {

			# Parse content
			df <- read.table(text = single.content, header = TRUE, row.names = NULL, sep = "\t", quote = '', stringsAsFactors = FALSE)

			if (nrow(df) > 0) {

				# Determine which columns contain constant value
				entry.fields <- colnames(df)[vapply(colnames(df), function(x) sum(! duplicated(x)) == 1, FUN.VALUE = TRUE)]

				# Remove peak columns from those columns (case where zero or only one peak in the table)
				entry.fields <- entry.fields[ ! entry.fields %in% c(BIODB.PEAK.MZEXP, BIODB.PEAK.MZTHEO, BIODB.PEAK.COMP, BIODB.PEAK.ATTR)]

				# Set entry fields
				for (f in entry.fields)
					entry$setFieldValue(f, df[1, f])

				# Make peak table
				peaks <- df[, ! colnames(df) %in% entry.fields]
				entry$setFieldValue(BIODB.PEAKS, peaks)
			}
		}

		entries <- c(entries, entry)
	}

	# Replace elements with no accession id by NULL
	entries <- lapply(entries, function(x) if (is.na(x$getField(BIODB.ACCESSION))) NULL else x)

	# If the input was a single element, then output a single object
	if (drop && length(content) == 1)
		entries <- entries[[1]]

	return(entries)
})
