if ( ! exists('KeggConn')) { # Do not load again if already loaded

	source('BiodbConn.R')
	source('KeggCompound.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	KeggConn <- setRefClass("KeggConn", contains = "BiodbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	KeggConn$methods( getEntryContentType = function(type) {
		return(RBIODB.TXT)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################

	KeggConn$methods( getEntryContent = function(type, id) {

		if (type == RBIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(id))

			# Request
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.entry.url(RBIODB.KEGG, x, content.type = RBIODB.TXT)), FUN.VALUE = '')

			return(content)
		}

		return(NULL)
	})

	################
	# CREATE ENTRY #
	################

	KeggConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == RBIODB.COMPOUND) createKeggCompoundFromTxt(content, drop = drop) else NULL)
	})

} # end of load safe guard
