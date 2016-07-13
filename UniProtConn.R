if ( ! exists('UniprotConn')) { # Do not load again if already loaded

	source('RemotedbConn.R')
	source('UniprotCompound.R')

	#####################
	# CLASS DECLARATION #
	#####################

	UniprotConn <- setRefClass("UniprotConn", contains = "RemotedbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	UniprotConn$methods( getEntryContentType = function(type) {
		return(BIODB.XML)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	UniprotConn$methods( getEntryContent = function(type, id) {

		if (type == BIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(id))

			# Request
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.entry.url(BIODB.UNIPROT, x, content.type = BIODB.XML)), FUN.VALUE = '')

			return(content)
		}

		return(NULL)
	})
	
	################
	# CREATE ENTRY #
	################
	
	UniprotConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == BIODB.COMPOUND) createUniprotCompoundFromXml(content, drop = drop) else NULL)
	})
}
