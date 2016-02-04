if ( ! exists('KeggConn')) { # Do not load again if already loaded

	source('BiodbConn.R')
	source('KeggCompound.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	KeggConn <- setRefClass("KeggConn", contains = "BiodbConn")

	#######################################
	# GET TYPE OF DOWNLOADABLE COMPOUND FILE #
	#######################################

	KeggConn$methods( getTypeOfDownloadableCompoundFile = function() {
		return(RBIODB.TXT)
	})
	
	###############################
	# DOWNLOAD COMPOUND FILE CONTENT #
	###############################
	
	# Download a compound description as a file content, from the public database.
	# id        The ID of the compound for which to download file content.
	# RETURN    The file content describing the compound.
	KeggConn$methods(.doDownloadCompoundFileContent = function(id) {
			url <- get.kegg.compound.url(id, txt = TRUE)
			txt <- .self$.getUrl(url)
			return(txt)
	})
	
	################
	# CREATE COMPOUND #
	################
	
	# Creates a Compound instance from file content.
	# file_content  A file content, downloaded from the public database.
	# RETURN        A compound instance.
	KeggConn$methods( .doCreateCompound = function(file_content) {
		compound <- createKeggCompoundFromText(file_content)
		return(compound)
	})
	
	######################
	# GET KEGG COMPOUND URL #
	######################
	
	get.kegg.compound.url <- function(id, txt = FALSE) {

		if (txt)
			url <- paste0('http://rest.kegg.jp/get/', id)
		else
			url <- paste0('http://www.genome.jp/dbget-bin/www_bget?cpd:', id)
	
		return(url)
	}

} # end of load safe guard
