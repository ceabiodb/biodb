if ( ! exists('get.pubchem.compound.url')) { # Do not load again if already loaded

	source('BiodbConn.R')
	source('PubchemCompound.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	PubchemConn <- setRefClass("PubchemConn", contains = "BiodbConn")

	#######################################
	# GET TYPE OF DOWNLOADABLE COMPOUND FILE #
	#######################################

	PubchemConn$methods( getTypeOfDownloadableCompoundFile = function() {
		return(RBIODB.XML)
	})
	
	###############################
	# DOWNLOAD COMPOUND FILE CONTENT #
	###############################

	PubchemConn$methods( .doDownloadCompoundFileContent = function(id) {
		url <- get.pubchem.compound.url(id, type = RBIODB.XML)
		html <- .self$.getUrl(url)
		return(html)
	})
	
	################
	# CREATE COMPOUND #
	################

	PubchemConn$methods( .doCreateCompound = function(file_content) {
		compound <- createPubchemCompoundFromXml(file_content)
		return(compound)
	})

	#########################
	# GET PUBCHEM COMPOUND URL #
	#########################
	
	get.pubchem.compound.url <- function(id, type = RBIODB.HTML) {
	
		id <- gsub(' ', '', id, perl = TRUE)
		id <- gsub('^CID', '', id, perl = TRUE)

		if (type == RBIODB.XML)
			url <- paste0('http://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/', id, '/XML/?response_type=save&response_basename=CID_', id)
		else
			url <- paste0('http://pubchem.ncbi.nlm.nih.gov/compound/', id)
	
		return(url)
	}

	#########################
	# GET PUBCHEM IMAGE URL #
	#########################
	
	get.pubchem.image.url <- function(id) {
	
		url <- paste0('http://pubchem.ncbi.nlm.nih.gov/image/imgsrv.fcgi?cid=', id, '&t=l')

		return(url)
	}
	
} # end of load safe guard
