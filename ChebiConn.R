if ( ! exists('ChebiConn')) { # Do not load again if already loaded

	source('BioDbConn.R')
#	source('ChebiEntry.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	ChebiConn <- setRefClass("ChebiConn", contains = "BioDbConn")
	
	###############################
	# DOWNLOAD ENTRY FILE CONTENT #
	###############################
	
	# Download an entry description as a file content, from the public database.
	# id        The ID of the entry for which to download file content.
	# RETURN    The file content describing the entry.
	ChebiConn$methods( .doDownloadEntryFileContent = function(id) {
		url <- get.chebi.entry.url(id)
		html <- .self$.getUrl(url)
		return(html)
	})
	
	################
	# CREATE ENTRY #
	################
	
	# Creates an Entry instance from file content.
	# file_content  A file content, downloaded from the public database.
	# RETURN        An Entry instance.
	ChebiConn$methods( .doCreateEntry = function(file_content) {
#entry <- createChebiEntryFromXml(file_content)
		return(entry)
	})

	#######################
	# GET CHEBI ENTRY URL #
	#######################
	
	get.chebi.entry.url <- function(id) {
	
		url <- paste0('https://www.ebi.ac.uk/chebi/searchId.do?chebiId=', id)
	
		return(url)
	}

} # end of load safe guard
