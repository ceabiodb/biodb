if ( ! exists('BiodbConn')) { # Do not load again if already loaded

	source('../r-lib/UrlRequestScheduler.R', chdir = TRUE)
	
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
	
	#############
	# GET ENTRY #
	#############
	
	# Get an entry from the public database.
	# id        The ID of the entry to get.
	# RETURN    An Entry instance.
	BiodbConn$methods( getEntry = function(id, factory = NULL) {
		content <- .self$downloadEntryFileContent(id)
		return(.self$createEntry(content, factory = factory))
	})
	
	#######################################
	# GET TYPE OF DOWNLOADABLE ENTRY FILE #
	#######################################
	
	BiodbConn$methods( getTypeOfDownloadableEntryFile = function() {
		stop("Method getTypeOfDownloadableEntryFile() is not implemented in concrete class.")
	})
	
	###############################
	# DOWNLOAD ENTRY FILE CONTENT #
	###############################
	
	# Download an entry description as a file content, from the public database.
	# id        The ID of the entry for which to download file content.
	# save_as   If set saves the content into the specified file.
	# RETURN    The file content describing the entry.
	BiodbConn$methods( downloadEntryFileContent = function(id, save_as = NA_character_) {
	
		# Download content
		content <- .self$.doDownloadEntryFileContent(id)
		if ( ! is.null(content) && ! is.null(save_as) && ! is.na(save_as)) {
			fileConn<-file(save_as)
			writeLines(content, fileConn)
			close(fileConn)
		}
	
		# Return content
		return(content)
	})
	
	# Download an entry description as a file content, from the public database.
	# This method has to be overwritten by sub-classes.
	# id        The ID of the entry for which to download file content.
	# RETURN    The file content describing the entry.
	BiodbConn$methods(.doDownloadEntryFileContent = function(id) {
		stop("Method .doDownloadEntryFileContent() is not implemented in concrete class.")
	})
	
	################
	# CREATE ENTRY #
	################
	
	# Creates an Entry instance from file content.
	# content       A file content, downloaded from the public database.
	# RETURN        An Entry instance.
	BiodbConn$methods( createEntry = function(content, factory = NULL) {

		# Create entry
		entry <- NULL
		if ( ! is.null(content) && ! is.na(content) && nchar(content) > 0) {
			entry <- .self$.doCreateEntry(content)
			if ( ! is.null(entry))
				entry$setFactory(factory)
		}

		return(entry)
	})
	
	# Creates an Entry instance from file content.
	# This method has to be overwritten by sub-classes.
	# content       A file content, downloaded from the public database.
	# RETURN    The file content describing the entry.
	BiodbConn$methods(
		.doCreateEntry = function(content) {
			return(NULL)
	})
}
