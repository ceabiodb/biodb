if ( ! exists('get.pubchem.compound.url')) { # Do not load again if already loaded

	source('RemotedbConn.R')
	source('PubchemCompound.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	PubchemConn <- setRefClass("PubchemConn", contains = "RemotedbConn", fields = list( .db = "character" ))

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	PubchemConn$methods( getEntryContentType = function(type) {
		return(BIODB.XML)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	PubchemConn$methods( getEntryContent = function(type, ids) {

		# Debug
		.self$.print.debug.msg(paste0("Get entry content(s) for ", length(ids)," id(s)..."))

		if (type == BIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(ids))

			# Request
			content <- vapply(ids, function(x) .self$.scheduler$getUrl(get.entry.url(BIODB.PUBCHEM, x, content.type = BIODB.XML)), FUN.VALUE = '')

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
	
	get.pubchem.image.url <- function(id, db = BIODB.PUBCHEMCOMP) {
	
		url <- paste0('http://pubchem.ncbi.nlm.nih.gov/image/imgsrv.fcgi?', (if (db == BIODB.PUBCHEMCOMP) 'cid' else 'sid'), '=', id, '&t=l')

		return(url)
	}
	
} # end of load safe guard
