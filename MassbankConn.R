if ( ! exists('MassbankConn')) { # Do not load again if already loaded

	source('BiodbConn.R')
	source('MassbankCompound.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	MassbankConn <- setRefClass("MassbankConn", contains = "BiodbConn")

	#######################################
	# GET TYPE OF DOWNLOADABLE COMPOUND FILE #
	#######################################

	MassbankConn$methods( getTypeOfDownloadableCompoundFile = function() {
		return(RBIODB.TXT)
	})
	
	###############################
	# DOWNLOAD COMPOUND FILE CONTENT #
	###############################
	
	# Download an compound description as a file content, from the public database.
	# id        The ID of the compound for which to download file content.
	# RETURN    The file content describing the compound.
	MassbankConn$methods( .doDownloadCompoundFileContent = function(id) {
		url <- get.massbank.compound.url(id)
		html <- .self$.getUrl(url)
		return(html)
	})
	
	################
	# CREATE COMPOUND #
	################
	
	# Creates an Compound instance from file content.
	# file_content  A file content, downloaded from the public database.
	# RETURN        An Compound instance.
	MassbankConn$methods( .doCreateCompound = function(file_content) {
		compound <- createMassbankCompoundFromTxt(file_content)
		return(compound)
	})

	##########################
	# GET MASSBANK COMPOUND URL #
	##########################
	
	get.massbank.compound.url <- function(id) {
	
		url <- paste0('http://www.massbank.jp/api/services/MassBankAPI/getRecordInfo?ids=', id)
	
		return(url)
	}

} # end of load safe guard

