if ( ! exists('get.pubchem.entry.url')) { # Do not load again if already loaded

	source('BiodbConn.R')
	source('PubchemEntry.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	PubchemConn <- setRefClass("PubchemConn", contains = "BiodbConn")

	#######################################
	# GET TYPE OF DOWNLOADABLE ENTRY FILE #
	#######################################

	PubchemConn$methods( getTypeOfDownloadableEntryFile = function() {
		return(RBIODB.XML)
	})
	
	###############################
	# DOWNLOAD ENTRY FILE CONTENT #
	###############################

	PubchemConn$methods( .doDownloadEntryFileContent = function(id) {
		url <- get.pubchem.entry.url(id, type = RBIODB.XML)
		html <- .self$.getUrl(url)
		return(html)
	})
	
	################
	# CREATE ENTRY #
	################

	PubchemConn$methods( .doCreateEntry = function(file_content) {
		entry <- createPubchemEntryFromXml(file_content)
		return(entry)
	})

	#########################
	# GET PUBCHEM ENTRY URL #
	#########################
	
	get.pubchem.entry.url <- function(id, type = RBIODB.HTML) {
	
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
