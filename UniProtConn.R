if ( ! exists('UniprotConn')) { # Do not load again if already loaded

	source('BiodbConn.R')
	source('UniprotCompound.R')

	#####################
	# CLASS DECLARATION #
	#####################

	UniprotConn <- setRefClass("UniprotConn", contains = "BiodbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	UniprotConn$methods( getEntryContentType = function(type) {
		return(RBIODB.XML)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	UniprotConn$methods( getEntryContent = function(type, id) {

		if (type == RBIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(id))

			# Request
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.entry.url(RBIODB.UNIPROT, x, content.type = RBIODB.XML)), FUN.VALUE = '')

			return(content)
		}

		return(NULL)
	})
	
	################
	# CREATE ENTRY #
	################
	
	UniprotConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == RBIODB.COMPOUND) createUniprotCompoundFromXml(content, drop = drop) else NULL)
	})
}
