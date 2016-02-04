if ( ! exists('MassbankConn')) { # Do not load again if already loaded

	source('BiodbConn.R')
	source('MassbankEntry.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	MassbankConn <- setRefClass("MassbankConn", contains = "BiodbConn")

	#######################################
	# GET TYPE OF DOWNLOADABLE ENTRY FILE #
	#######################################

	MassbankConn$methods( getTypeOfDownloadableEntryFile = function() {
		return(RBIODB.TXT)
	})
	
	###############################
	# DOWNLOAD ENTRY FILE CONTENT #
	###############################
	
	# Download an entry description as a file content, from the public database.
	# id        The ID of the entry for which to download file content.
	# RETURN    The file content describing the entry.
	MassbankConn$methods( .doDownloadEntryFileContent = function(id) {
		url <- get.massbank.entry.url(id)
		html <- .self$.getUrl(url)
		return(html)
	})
	
	################
	# CREATE ENTRY #
	################
	
	# Creates an Entry instance from file content.
	# file_content  A file content, downloaded from the public database.
	# RETURN        An Entry instance.
	MassbankConn$methods( .doCreateEntry = function(file_content) {
		entry <- createMassbankEntryFromTxt(file_content)
		return(entry)
	})

	##########################
	# GET MASSBANK ENTRY URL #
	##########################
	
	get.massbank.entry.url <- function(id) {
	
		url <- paste0('http://www.massbank.jp/api/services/MassBankAPI/getRecordInfo?ids=', id)
	
		return(url)
	}

} # end of load safe guard

