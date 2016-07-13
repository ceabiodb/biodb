if ( ! exists('FiledbConn')) {

	source('BiodbConn.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	FiledbConn <- setRefClass("FiledbConn", contains = "BiodbConn", fields = list(.file = "character",.file.sep = "character", .file.quote = "character", .db = "ANY", .fields = "list"))

	###############
	# CONSTRUCTOR #
	###############

	FiledbConn$methods( initialize = function(file = NA_character_, file.sep = "\t", file.quote = "\"", ...) {

		# Check file
		(! is.null(file) && is.na(file)) || stop("You must specify a file database to load.")
		file.exists(file) || stop(paste0("Cannot locate the file database \"", file ,"\"."))

		# Set fields
		.file <<- file
		.file.sep <<- file.sep
		.file.quote <<- file.quote
		.fields <<- BIODB.DFT.DB.FIELDS

		callSuper(...)
	})

	##############
	# SET FIELDS #
	##############

	FiledbConn$methods( setFields = function(fields) {
		                   # TODO
	})

	################
	# SET MS MODES #
	################

	FiledbConn$methods( setMsModes = function(modes) {
		                   # TODO
	})

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	FiledbConn$methods( getEntryContentType = function(type) {
		return(RBIODB.DATAFRAME)
	})

	###########
	# INIT DB #
	###########

	FiledbConn$methods( .init.db = function() {

		if (is.null(.self$.db)) {

			# Load database
			.db <<- read.table(.self$.file, sep = .self$.file.sep, .self$.file.quote, header = TRUE, stringsAsFactors = FALSE, row.names = NULL)

			# Rename columns
			colnames(.self$.db) <- vapply(colnames(.self$.db), function(c) if (c %in% .self$.fields) names(.self$.fields)[.self$.fields %in% c] else c, FUN.VALUE = '')
		}
	})

}
