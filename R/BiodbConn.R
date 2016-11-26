if ( ! exists('BiodbConn')) {

	#####################
	# CLASS DECLARATION #
	#####################
	
	BiodbConn <- setRefClass("BiodbConn", fields = list( .debug = "logical" ))

	###############
	# CONSTRUCTOR #
	###############

	BiodbConn$methods( initialize = function(debug = FALSE, ...) {
		.debug <<- debug
	})

	#######################
	# PRINT DEBUG MESSAGE #
	#######################

	BiodbConn$methods( .print.debug.msg = function(msg) {
		if (.self$.debug)
			.print.msg(msg = msg, class = class(.self))
	})

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	BiodbConn$methods( getEntryContentType = function() {
		stop("Method getEntryContentType() is not implemented in concrete class.")
	})

	#############
	# GET ENTRY #
	#############

	BiodbConn$methods( getEntry = function(id, drop = TRUE) {
		content <- .self$getEntryContent(id)
		return(.self$createEntry(content, drop = drop))
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	# Download entry content from the public database.
	# type      The entry type.
	# id        The ID of the entry to get.
	# RETURN    An entry content downloaded from database.
	BiodbConn$methods( getEntryContent = function(id) {
		stop("Method getEntryContent() is not implemented in concrete class.")
	})
	
	#############################
	# CREATE ENTRY FROM CONTENT #
	#############################
	
	# Creates a Compound instance from file content.
	# content       A file content, downloaded from the public database.
	# RETURN        A compound instance.
	BiodbConn$methods( createEntry = function(content, drop = TRUE) {
		stop("Method createEntry() is not implemented in concrete class.")
	})

	#################
	# GET ENTRY IDS #
	#################
	
	# Get a list of IDs of all entries contained in this database.
	BiodbConn$methods( getEntryIds = function(max.results = NA_integer_) {
		stop("Method getEntryIds() is not implemented in concrete class.")
	})

	##################
	# GET NB ENTRIES #
	##################
	
	# Get the number of entries contained in this database.
	BiodbConn$methods( getNbEntries = function() {
		stop("Method getNbEntries() is not implemented in concrete class.")
	})
}
