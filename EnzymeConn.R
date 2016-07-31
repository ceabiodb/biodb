if ( ! exists('EnzymeConn')) { # Do not load again if already loaded

	source('RemotedbConn.R')
	source('EnzymeCompound.R')

	#####################
	# CLASS DECLARATION #
	#####################

	EnzymeConn <- setRefClass("EnzymeConn", contains = "RemotedbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	EnzymeConn$methods( getEntryContentType = function(type) {
		return(BIODB.TXT)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	EnzymeConn$methods( getEntryContent = function(type, id) {

		if (type == BIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(id))

			# Request
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.entry.url(BIODB.ENZYME, accession = x, content.type = BIODB.TXT)), FUN.VALUE = '')

			return(content)
		}

		return(NULL)
	})

	################
	# CREATE ENTRY #
	################
	
	EnzymeConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == BIODB.COMPOUND) createEnzymeCompoundFromTxt(content, drop = drop) else NULL)
	})
}
