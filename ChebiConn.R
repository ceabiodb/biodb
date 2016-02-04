if ( ! exists('ChebiConn')) { # Do not load again if already loaded

	source('BiodbConn.R')
	source('ChebiCompound.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	ChebiConn <- setRefClass("ChebiConn", contains = "BiodbConn")

	#######################################
	# GET TYPE OF DOWNLOADABLE COMPOUND FILE #
	#######################################

	ChebiConn$methods( getTypeOfDownloadableCompoundFile = function() {
		return(RBIODB.HTML)
	})
	
	###############################
	# DOWNLOAD COMPOUND FILE CONTENT #
	###############################
	
	# Download a compound description as a file content, from the public database.
	# id        The ID of the compound for which to download file content.
	# RETURN    The file content describing the compound.
	ChebiConn$methods( .doDownloadCompoundFileContent = function(id) {
		url <- get.chebi.compound.url(id)
		html <- .self$.getUrl(url)
		return(html)
	})
	
	################
	# CREATE COMPOUND #
	################
	
	# Creates a Compound instance from file content.
	# file_content  A file content, downloaded from the public database.
	# RETURN        A compound instance.
	ChebiConn$methods( .doCreateCompound = function(file_content) {
		compound <- createChebiCompoundFromHtml(file_content)
		return(compound)
	})

	#######################
	# GET CHEBI COMPOUND URL #
	#######################
	
	get.chebi.compound.url <- function(id) {
	
		url <- paste0('https://www.ebi.ac.uk/chebi/searchId.do?chebiId=', id)
	
		return(url)
	}

} # end of load safe guard
