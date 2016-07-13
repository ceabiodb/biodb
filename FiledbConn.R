if ( ! exists('FiledbConn')) {

	source('BiodbConn.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	FiledbConn <- setRefClass("FiledbConn", contains = "BiodbConn", fields = list(.file = "character"))

	###############
	# CONSTRUCTOR #
	###############

	FiledbConn$methods( initialize = function(file = NA_character_, ...) {

		# Check file
		(! is.null(file) && is.na(file)) || stop("You must specify a file database to load.")
		file.exists(file) || stop(paste0("Cannot locate the file database \"", file ,"\"."))
		.file <<- file

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
		return(RBIODB.HTML)
	})
}
