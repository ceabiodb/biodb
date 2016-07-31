if ( ! exists('BiodbConn')) {

	source('biodb-common.R')

	#####################
	# CLASS DECLARATION #
	#####################
	
	BiodbConn <- setRefClass("BiodbConn")

	######################
	# HANDLES ENTRY TYPE #
	######################

	BiodbConn$methods( handlesEntryType = function(type) {
		return( ! is.null(.self$getEntryContentType(type)))
	})

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	BiodbConn$methods( getEntryContentType = function(type) {
		stop("Method getEntryContentType() is not implemented in concrete class.")
	})

	#############
	# GET ENTRY #
	#############

	BiodbConn$methods( getEntry = function(type, id, drop = TRUE) {
		content <- .self$getEntryContent(type, id)
		return(.self$createEntry(type, content, drop = drop))
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	# Download entry content from the public database.
	# type      The entry type.
	# id        The ID of the entry to get.
	# RETURN    An entry content downloaded from database.
	BiodbConn$methods( getEntryContent = function(type, id) {
		stop("Method getCompound() is not implemented in concrete class.")
	})
	
	#############################
	# CREATE ENTRY FROM CONTENT #
	#############################
	
	# Creates a Compound instance from file content.
	# content       A file content, downloaded from the public database.
	# RETURN        A compound instance.
	BiodbConn$methods( createEntry = function(type, content, drop = TRUE) {
		stop("Method createEntry() is not implemented in concrete class.")
	})

	#################
	# GET ENTRY IDS #
	#################
	
	# Get a list of IDs of all entries contained in this database.
	BiodbConn$methods( getEntryIds = function(type) {
		stop("Method getEntryIds() is not implemented in concrete class.")
	})

	##################
	# GET NB ENTRIES #
	##################
	
	# Get the number of entries contained in this database.
	BiodbConn$methods( getNbEntries = function(type) {
		stop("Method getNbEntries() is not implemented in concrete class.")
	})
}
