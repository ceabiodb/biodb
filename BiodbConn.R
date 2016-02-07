if ( ! exists('BiodbConn')) { # Do not load again if already loaded

	source(file.path('..', 'r-lib', 'UrlRequestScheduler.R'), chdir = TRUE)

	#############
	# CONSTANTS #
	#############
	
	# Entry types
	RBIODB.COMPOUND <- 'compound'
	RBIODB.SPECTRUM <- 'spectrum'
	
	# Entry content types
	RBIODB.HTML <- 'html'
	RBIODB.TXT  <- 'text'
	RBIODB.XML  <- 'xml'

	#####################
	# CLASS DECLARATION #
	#####################
	
	BiodbConn <- setRefClass("BiodbConn", fields = list(.factory = "BiodbFactory", .scheduler = "UrlRequestScheduler"))

	###############
	# CONSTRUCTOR #
	###############

	BiodbConn$methods( initialize = function(factory = NULL, scheduler = NULL, ...) {

		# Set factory
		! is.null(factory) || stop("You must provide a factory class. You should use the Factory class to create an instance of a connection class.")
		inherits(factory, "BiodbFactory") || stop("The factory instance must inherit from BiodbFactory class.")
		.factory <<- factory

		# Set scheduler
		if (is.null(scheduler))
			scheduler UrlRequestScheduler$new(n = 3)
		inherits(scheduler, "UrlRequestScheduler") || stop("The scheduler instance must inherit from UrlRequestScheduler class.")
		scheduler$setUserAgent(factory$getUserAgent()) # set agent
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

	BiodbConn$methods( handlesEntryType = function(type) {
		return(type %in% names(.self$.entry.types))
	})

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	BiodbConn$methods( getEntryContentType = function(type) {

		if (type %in% names(.self$.entry.types))
			return(.self$.entry.types[[type]]$content.type)

		return(NULL)
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
#		content <- .self$downloadCompoundFileContent(id)
#		return(.self$createCompound(content, factory = factory))
	})

	################
	# GET SPECTRUM #
	################
	
	# Get a spectrum from the public database.
	# id        The ID of the spectrum to get.
	# RETURN    A spectrum file. 
	BiodbConn$methods( getSpectrumFile = function(id) {
		stop("Method getSpectrum() is not implemented in concrete class.")
#		content <- .self$downloadSpectrumFileContent(id)
#		return(.self$createSpectrum(content, factory = factory))
	})

	##########################################
	# GET TYPE OF DOWNLOADABLE COMPOUND FILE #
	##########################################
	
	BiodbConn$methods( getCompoundFileType = function() {
		stop("Method getTypeOfDownloadableCompoundFile() is not implemented in concrete class.")
	})
	
	##################################
	# DOWNLOAD COMPOUND FILE CONTENT #
	##################################
	
	# Download a compound description as a file content, from the public database.
	# id        The ID of the compound for which to download file content.
	# save_as   If set saves the content into the specified file.
	# RETURN    The file content describing the compound.
	BiodbConn$methods( downloadCompoundFileContent = function(id, save_as = NA_character_) {
	
		# Download content
		content <- .self$.doDownloadCompoundFileContent(id)
		if ( ! is.null(content) && ! is.null(save_as) && ! is.na(save_as)) {
			fileConn<-file(save_as)
			writeLines(content, fileConn)
			close(fileConn)
		}
	
		# Return content
		return(content)
	})
	
	# Download a compound description as a file content, from the public database.
	# This method has to be overwritten by sub-classes.
	# id        The ID of the compound for which to download file content.
	# RETURN    The file content describing the compound.
	BiodbConn$methods(.doDownloadCompoundFileContent = function(id) {
		stop("Method .doDownloadCompoundFileContent() is not implemented in concrete class.")
	})
	
	###################
	# CREATE COMPOUND #
	###################
	
	# Creates a Compound instance from file content.
	# content       A file content, downloaded from the public database.
	# RETURN        A compound instance.
	BiodbConn$methods( createCompound = function(content) {

		# Create compound
		compound <- NULL
		if ( ! is.null(content) && ! is.na(content) && nchar(content) > 0) {
			compound <- .self$.doCreateCompound(content)
			if ( ! is.null(compound))
				compound$setFactory(factory)
		}

		return(compound)
	})
	
	# Creates a Compound instance from file content.
	# This method has to be overwritten by sub-classes.
	# content   A file content, downloaded from the public database.
	# RETURN    A compound instance.
	BiodbConn$methods(
		.doCreateCompound = function(content) {
			return(NULL)
	})
}
