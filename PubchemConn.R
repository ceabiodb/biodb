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
		return(RBIODB.XML)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	PubchemConn$methods( getEntryContent = function(type, id) {

		if (type == RBIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(id))

			# Request
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.entry.url(RBIODB.PUBCHEM, x, content.type = RBIODB.XML)), FUN.VALUE = '')

			return(content)
		}

		return(NULL)
	})
	
	################
	# CREATE ENTRY #
	################
	
	PubchemConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == RBIODB.COMPOUND) createPubchemCompoundFromXml(content, drop = drop) else NULL)
	})

	#########################
	# GET PUBCHEM IMAGE URL #
	#########################
	
	get.pubchem.image.url <- function(id) {
	
		url <- paste0('http://pubchem.ncbi.nlm.nih.gov/image/imgsrv.fcgi?cid=', id, '&t=l')

		return(url)
	}
	
} # end of load safe guard
