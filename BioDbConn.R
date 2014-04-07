source('../r-lib/UrlRequestScheduler.R', chdir = TRUE)

#####################
# CLASS DECLARATION #
#####################

BioDbConn <- setRefClass("BioDbConn",
						   fields = list(useragent = "character", scheduler="UrlRequestScheduler"))

###############
# CONSTRUCTOR #
###############

BioDbConn$methods( initialize = function(useragent = "", scheduler = UrlRequestScheduler$new(n = 3), ...) {
	useragent <<- useragent
	scheduler <<- scheduler
	callSuper(...) # calls super-class initializer with remaining parameters
})

###########
# GET URL #
###########

# Get an url content, using scheduler.
# url       The URL to download.
# RETURN    The downloaded content.
BioDbConn$methods( .getUrl = function(url) {
	return(.self$scheduler$getUrl(url, useragent = .self$useragent))
})

#############
# GET ENTRY #
#############

# Get an entry from the public database.
# id        The ID of the entry to get.
# RETURN    An Entry instance.
BioDbConn$methods(
	getEntry = function(id) {
		return(.self$createEntry(.self$downloadEntryFileContent(id)))
})

###############################
# DOWNLOAD ENTRY FILE CONTENT #
###############################

# Download an entry description as a file content, from the public database.
# id        The ID of the entry for which to download file content.
# save_as   If set saves the content into the specified file.
# RETURN    The file content describing the entry.
BioDbConn$methods(
	downloadEntryFileContent = function(id, save_as = NA_character_) {

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
# id        The ID of the entry for which to download file content.
# RETURN    The file content describing the entry.
BioDbConn$methods(
	.doDownloadEntryFileContent = function(id) {
		return(NULL)
})

################
# CREATE ENTRY #
################

# Creates an Entry instance from file content.
# file_content  A file content, downloaded from the public database.
# RETURN        An Entry instance.
BioDbConn$methods(
	createEntry = function(file_content) {
		return(NULL)
})
