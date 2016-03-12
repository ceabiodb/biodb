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
	
#	###########
#	# GET URL #
#	###########
#
#	# Get an url content, using scheduler.
#	# url       The URL to download.
#	# RETURN    The downloaded content.
#	BiodbConn$methods( .getUrl = function(url, params = NULL, method = RLIB.GET) {
#		return(.self$.scheduler$getUrl(url, params = params, method = method))
#	})

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
	
#	##################################
#	# DOWNLOAD COMPOUND FILE CONTENT #
#	##################################
#	
#	# Download a compound description as a file content, from the public database.
#	# id        The ID of the compound for which to download file content.
#	# save_as   If set saves the content into the specified file.
#	# RETURN    The file content describing the compound.
#	BiodbConn$methods( downloadCompoundFileContent = function(id, save_as = NA_character_) {
#	
#		# Download content
#		content <- .self$.doDownloadCompoundFileContent(id)
#		if ( ! is.null(content) && ! is.null(save_as) && ! is.na(save_as)) {
#			fileConn<-file(save_as)
#			writeLines(content, fileConn)
#			close(fileConn)
#		}
#	
#		# Return content
#		return(content)
#	})
	
#	# Download a compound description as a file content, from the public database.
#	# This method has to be overwritten by sub-classes.
#	# id        The ID of the compound for which to download file content.
#	# RETURN    The file content describing the compound.
#	BiodbConn$methods(.doDownloadCompoundFileContent = function(id) {
#		stop("Method .doDownloadCompoundFileContent() is not implemented in concrete class.")
#	})
	
	#############################
	# CREATE ENTRY FROM CONTENT #
	#############################
	
	# Creates a Compound instance from file content.
	# content       A file content, downloaded from the public database.
	# RETURN        A compound instance.
	BiodbConn$methods( createEntry = function(type, content, drop = TRUE) {
		stop("Method createEntry() is not implemented in concrete class.")

#		# Create compound
#		compound <- NULL
#		if ( ! is.null(content) && ! is.na(content) && nchar(content) > 0) {
#			compound <- .self$.doCreateCompound(content)
##			if ( ! is.null(compound))
##				compound$setFactory(factory)
#		}
#
#		return(compound)
	})
	
#	# Creates a Compound instance from file content.
#	# This method has to be overwritten by sub-classes.
#	# content   A file content, downloaded from the public database.
#	# RETURN    A compound instance.
#	BiodbConn$methods(
#		.doCreateCompound = function(content) {
#			return(NULL)
#	})
}
