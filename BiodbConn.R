if ( ! exists('BiodbConn')) { # Do not load again if already loaded

	source(file.path('..', 'r-lib', 'UrlRequestScheduler.R'), chdir = TRUE)

	#############
	# CONSTANTS #
	#############
	
	RBIODB.HTML <- 'html'
	RBIODB.TXT  <- 'text'
	RBIODB.XML  <- 'xml'
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	BiodbConn <- setRefClass("BiodbConn", fields = list(.scheduler="UrlRequestScheduler"))
	
	###############
	# CONSTRUCTOR #
	###############
	
	BiodbConn$methods( initialize = function(useragent = NA_character_, scheduler = UrlRequestScheduler$new(n = 3), ...) {
		.scheduler <<- scheduler
		if ( ! is.null(useragent) && ! is.na(useragent) && ! nchar(useragent) == 0)
			.self$.scheduler$setUserAgent(useragent)
	
		callSuper(...) # calls super-class initializer with remaining parameters
	})
	
	###########
	# GET URL #
	###########

	# Get an url content, using scheduler.
	# url       The URL to download.
	# RETURN    The downloaded content.
	BiodbConn$methods( .getUrl = function(url, params = NULL, method = 'GET') {
		return(.self$.scheduler$getUrl(url, params = params, method = method))
	})

	################
	# GET COMPOUND #
	################
	
	# Get a compound from the public database.
	# id        The ID of the compound to get.
	# RETURN    A compound instance.
	BiodbConn$methods( getCompound = function(id, factory = NULL) {
		content <- .self$downloadCompoundFileContent(id)
		return(.self$createCompound(content, factory = factory))
	})

	################
	# GET SPECTRUM #
	################
	
	# Get a spectrum from the public database.
	# id        The ID of the spectrum to get.
	# RETURN    A spectrum instance.
	BiodbConn$methods( getSpectrum = function(id, factory = NULL) {
		content <- .self$downloadSpectrumFileContent(id)
		return(.self$createSpectrum(content, factory = factory))
	})

	##########################################
	# GET TYPE OF DOWNLOADABLE COMPOUND FILE #
	##########################################
	
	BiodbConn$methods( getTypeOfDownloadableCompoundFile = function() {
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
	BiodbConn$methods( createCompound = function(content, factory = NULL) {

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
