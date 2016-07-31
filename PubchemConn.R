if ( ! exists('get.pubchem.compound.url')) { # Do not load again if already loaded

	source('RemotedbConn.R')
	source('PubchemCompound.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	PubchemConn <- setRefClass("PubchemConn", contains = "RemotedbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	PubchemConn$methods( getEntryContentType = function(type) {
		return(BIODB.XML)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	PubchemConn$methods( getEntryContent = function(type, id) {

		if (type == BIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(id))

			# Request
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.entry.url(BIODB.PUBCHEM, x, content.type = BIODB.XML)), FUN.VALUE = '')

			return(content)
		}

		return(NULL)
	})
	
	################
	# CREATE ENTRY #
	################
	
	PubchemConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == BIODB.COMPOUND) createPubchemCompoundFromXml(content, drop = drop) else NULL)
	})

	#########################
	# GET PUBCHEM IMAGE URL #
	#########################
	
	get.pubchem.image.url <- function(id) {
	
		url <- paste0('http://pubchem.ncbi.nlm.nih.gov/image/imgsrv.fcgi?cid=', id, '&t=l')

		return(url)
	}
	
} # end of load safe guard
