if ( ! exists('HmdbConn')) { # Do not load again if already loaded

	source('BioDbConn.R')
	source('HmdbEntry.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	HmdbConn <- setRefClass("HmdbConn", contains = "BioDbConn")
	
	###############################
	# DOWNLOAD ENTRY FILE CONTENT #
	###############################
	
	# Download an entry description as a file content, from the public database.
	# id        The ID of the entry for which to download file content.
	# RETURN    The file content describing the entry.
	HmdbConn$methods(
		.doDownloadEntryFileContent = function(id) {
			url <- get.hmdb.entry.url(id, xml = TRUE)
			xml <- .self$.getUrl(url)
			return(xml)
	})
	
	################
	# CREATE ENTRY #
	################
	
	# Creates an Entry instance from file content.
	# file_content  A file content, downloaded from the public database.
	# RETURN        An Entry instance.
	HmdbConn$methods(
		.doCreateEntry = function(file_content) {
			entry <- createHmdbEntryFromXml(file_content)
			return(entry)
	})
	
	######################
	# GET HMDB ENTRY URL #
	######################
	
	get.hmdb.entry.url <- function(id, xml = FALSE) {
	
		url <- paste0('http://www.hmdb.ca/metabolites/', id)
	
		if (xml)
			url <- paste0(url, '.xml')
	
		return(url)
	}
	
} # end of load safe guard
