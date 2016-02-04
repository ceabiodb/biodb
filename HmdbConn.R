if ( ! exists('HmdbConn')) { # Do not load again if already loaded

	source('BiodbConn.R')
	source('HmdbCompound.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	HmdbConn <- setRefClass("HmdbConn", contains = "BiodbConn")
	
	###############################
	# DOWNLOAD COMPOUND FILE CONTENT #
	###############################
	
	# Download a compound description as a file content, from the public database.
	# id        The ID of the compound for which to download file content.
	# RETURN    The file content describing the compound.
	HmdbConn$methods(
		.doDownloadCompoundFileContent = function(id) {
			url <- get.hmdb.compound.url(id, xml = TRUE)
			xml <- .self$.getUrl(url)
			return(xml)
	})
	
	################
	# CREATE COMPOUND #
	################
	
	# Creates a Compound instance from file content.
	# file_content  A file content, downloaded from the public database.
	# RETURN        A compound instance.
	HmdbConn$methods(
		.doCreateCompound = function(file_content) {
			compound <- createHmdbCompoundFromXml(file_content)
			return(compound)
	})
	
	######################
	# GET HMDB COMPOUND URL #
	######################
	
	get.hmdb.compound.url <- function(id, xml = FALSE) {
	
		url <- paste0('http://www.hmdb.ca/metabolites/', id)
	
		if (xml)
			url <- paste0(url, '.xml')
	
		return(url)
	}
	
} # end of load safe guard
