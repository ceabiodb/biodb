if ( ! exists('BiodbConn')) { # Do not load again if already loaded

	source(file.path('..', 'r-lib', 'UrlRequestScheduler.R'), chdir = TRUE)
	source('common.R')

	#####################
	# CLASS DECLARATION #
	#####################
	
	BiodbConn <- setRefClass("BiodbConn", fields = list(.scheduler = "UrlRequestScheduler"))

	###############
	# CONSTRUCTOR #
	###############

	BiodbConn$methods( initialize = function(useragent = NA_character_, scheduler = NULL, ...) {

		# Check useragent
		! is.null(useragent) && ! is.na(useragent) || stop("You must specify a valid useragent.")

		# Set scheduler
		if (is.null(scheduler))
			scheduler <- UrlRequestScheduler$new(n = 3)
		inherits(scheduler, "UrlRequestScheduler") || stop("The scheduler instance must inherit from UrlRequestScheduler class.")
		scheduler$setUserAgent(useragent) # set agent
		.scheduler <<- scheduler
	
		callSuper(...) # calls super-class initializer with remaining parameters
	})
	
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
	# id        The ID of the enttry to get.
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
}
