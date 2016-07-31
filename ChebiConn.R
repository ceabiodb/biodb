if ( ! exists('ChebiConn')) { # Do not load again if already loaded

	source('RemotedbConn.R')
	source('ChebiCompound.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	ChebiConn <- setRefClass("ChebiConn", contains = "RemotedbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	ChebiConn$methods( getEntryContentType = function(type) {
		return(BIODB.HTML)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################

	ChebiConn$methods( getEntryContent = function(type, id) {

		if (type == BIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(id))

			# Request
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.entry.url(BIODB.CHEBI, x)), FUN.VALUE = '')

			return(content)
		}

		return(NULL)
	})
	
	################
	# CREATE ENTRY #
	################
	
	ChebiConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == BIODB.COMPOUND) createChebiCompoundFromHtml(content, drop = drop) else NULL)
	})

} # end of load safe guard
