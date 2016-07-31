if ( ! exists('KeggConn')) { # Do not load again if already loaded

	source('RemotedbConn.R')
	source('KeggCompound.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	KeggConn <- setRefClass("KeggConn", contains = "RemotedbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	KeggConn$methods( getEntryContentType = function(type) {
		return(BIODB.TXT)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################

	KeggConn$methods( getEntryContent = function(type, id) {

		if (type == BIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(id))

			# Request
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.entry.url(BIODB.KEGG, x, content.type = BIODB.TXT)), FUN.VALUE = '')

			return(content)
		}

		return(NULL)
	})

	################
	# CREATE ENTRY #
	################

	KeggConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == BIODB.COMPOUND) createKeggCompoundFromTxt(content, drop = drop) else NULL)
	})

} # end of load safe guard
