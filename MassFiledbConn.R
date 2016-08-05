if ( ! exists('MassFiledbConn')) {

	source('MassdbConn.R')
	
	# LCMS File db.
	# In this type of database, a single file is provided in CSV format. Default separator is tabulation.
	# Each line is a MS peak measure, .
	# The file contains molecule and spectrum information. Each spectrum has an accession id.

	# TODO Rename setField into setFieldName + addNewField, and setMsMode into setMsModeValue

	#############
	# CONSTANTS #
	#############

	# Default database fields
	.BIODB.DFT.DB.FIELDS <- list()
	for (f in c(BIODB.ACCESSION, BIODB.NAME, BIODB.FULLNAMES, BIODB.COMPOUND.ID, BIODB.MSMODE, BIODB.PEAK.MZ, BIODB.PEAK.COMP, BIODB.PEAK.ATTR, BIODB.CHROM.COL, BIODB.CHROM.COL.RT, BIODB.FORMULA, BIODB.MASS))
		.BIODB.DFT.DB.FIELDS[[f]] <- f

	#####################
	# CLASS DECLARATION #
	#####################
	
	MassFiledbConn <- setRefClass("MassFiledbConn", contains = "MassdbConn", fields = list(.file = "character", .file.sep = "character", .file.quote = "character", .field.multval.sep = 'character', .db = "ANY", .fields = "list", .ms.modes = "character"))

	###############
	# CONSTRUCTOR #
	###############

	MassFiledbConn$methods( initialize = function(file = NA_character_, file.sep = "\t", file.quote = "\"", ...) {

		# Check file
		(! is.null(file) && ! is.na(file)) || stop("You must specify a file database to load.")
		file.exists(file) || stop(paste0("Cannot locate the file database \"", file ,"\"."))

		# Set fields
		.db <<- NULL
		.file <<- file
		.file.sep <<- file.sep
		.file.quote <<- file.quote
		.fields <<- .BIODB.DFT.DB.FIELDS
		.field.multval.sep <<- ';'
		.ms.modes <<- c(BIODB.MSMODE.NEG, BIODB.MSMODE.POS)
		names(.self$.ms.modes) <- .self$.ms.modes

		callSuper(...)
	})

	######################
	# Is valid field tag #
	######################

	MassFiledbConn$methods( isValidFieldTag = function(tag) {
		return (tag %in% names(.self$.fields))
	})

	#############
	# Set field #
	#############

	MassFiledbConn$methods( setField = function(tag, colname) {

		( ! is.null(tag) && ! is.na(tag)) || stop("No tag specified.")
		( ! is.null(colname) && ! is.na(colname)) || stop("No column name specified.")

		# Load database file
		.self$.init.db()

		# Check that this field tag is defined in the fields list
		.self$isValidFieldTag(tag) || stop(paste0("Database field tag \"", tag, "\" is not valid."))

		# Check that columns are defined in database file
		all(colname %in% names(.self$.db)) || stop(paste0("One or more columns among ", paste(colname, collapse = ", "), " are not defined in database file."))

		# Set new definition
		if (length(colname) == 1)
			.fields[[tag]] <<- colname
		else {
			new.col <- paste(colname, collapse = ".")
			.self$.db[[new.col]] <- vapply(seq(nrow(.self$.db)), function(i) { paste(.self$.db[i, colname], collapse = '.') }, FUN.VALUE = '')
			.fields[[tag]] <<- new.col
		}
	})

	######################################
	# SET FIELD MULTIPLE VALUE SEPARATOR #
	######################################

	MassFiledbConn$methods( setFieldMultValSep = function(sep) {
		.field.multval.sep <<- sep
	})

	################
	# SET MS MODES #
	################

	MassFiledbConn$methods( setMsMode = function(mode, value) {
		.self$.ms.modes[[mode]] <- value
	})

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	MassFiledbConn$methods( getEntryContentType = function(type) {
		return(BIODB.DATAFRAME)
	})

	###########
	# INIT DB #
	###########

	MassFiledbConn$methods( .init.db = function() {

		if (is.null(.self$.db)) {

			# Load database
			.db <<- read.table(.self$.file, sep = .self$.file.sep, .self$.file.quote, header = TRUE, stringsAsFactors = FALSE, row.names = NULL)

			# Rename columns
			colnames(.self$.db) <- vapply(colnames(.self$.db), function(c) if (c %in% .self$.fields) names(.self$.fields)[.self$.fields %in% c] else c, FUN.VALUE = '')
		}
	})

	################
	# CHECK FIELDS #
	################

	MassFiledbConn$methods( .check.fields = function(fields) {

		# Check if fields are known
		unknown.fields <- names(.self$.fields)[ ! fields %in% names(.self$.fields)]
		if (length(unknown.fields) > 0)
			stop(paste0("Field(s) ", paste(fields, collapse = ", "), " is/are unknown."))

		# Init db
		.self$.init.db()

		# Check if fields are defined in file database
		undefined.fields <- colnames(.self$.init.db)[ ! unlist(.self$.fields[fields]) %in% colnames(.self$.init.db)]
		if (length(undefined.fields) > 0)
			stop(paste0("Column(s) ", paste(unlist(.self$.fields[fields]), collapse = ", "), " is/are undefined in file database."))
	})

	################
	# EXTRACT COLS #
	################
	
	MassFiledbConn$methods( .extract.cols = function(cols, mode = NULL, drop = FALSE, uniq = FALSE, sort = FALSE, max.rows = NA_integer_) {
	
		x <- NULL

		if ( ! is.null(cols) && ! is.na(cols)) {

			# Init db
			.self$.init.db()

			# TODO check existence of cols/fields

			# Get db, eventually filtering it.
			if (is.null(mode))
				db <- .self$.db
			else {
				# Check mode value
				mode %in% names(.self$.ms.modes) || stop(paste0("Unknown mode value '", mode, "'."))
				.self$.check.fields(BIODB.MSMODE)

				# Filter on mode
				db <- .self$.db[.self$.db[[unlist(.self$.fields[BIODB.MSMODE])]] %in% .self$.ms.modes[[mode]], ]
			}

			# Get subset
			x <- db[, unlist(.self$.fields[cols]), drop = drop]

			# Rename columns
			if (is.data.frame(x))
				colnames(x) <- cols

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
		}

		return(x)
	})

	#################
	# GET ENTRY IDS #
	#################
	
	MassFiledbConn$methods( getEntryIds = function(type) {

		ids <- NA_character_

		if (type %in% c(BIODB.SPECTRUM, BIODB.COMPOUND))
			ids <- as.character(.self$.extract.cols(if (type == BIODB.SPECTRUM) BIODB.ACCESSION else BIODB.COMPOUND.ID, drop = TRUE, uniq = TRUE, sort = TRUE))

		return(ids)
	})

	##################
	# GET NB ENTRIES #
	##################
	
	MassFiledbConn$methods( getNbEntries = function(type) {
		return(length(.self$getEntryIds(type)))
	})

	###############################
	# GET CHROMATOGRAPHIC COLUMNS #
	###############################
	
	# Inherited from MassdbConn.
	MassFiledbConn$methods( getChromCol = function(compound.ids = NULL) {

		# Extract needed columns
		db <- .self$.extract.cols(c(BIODB.COMPOUND.ID, BIODB.CHROM.COL))

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
	
	#################
	# GET MZ VALUES #
	#################
	
	# Inherited from MassdbConn.
	MassFiledbConn$methods( getMzValues = function(mode = NULL, max.results = NA_integer_) {

		# Get mz values
		mz <- .self$.extract.cols(BIODB.PEAK.MZ, mode = mode, drop = TRUE, uniq = TRUE, sort = TRUE, max.rows = max.results)

		return(mz)
	})

}
