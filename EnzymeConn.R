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
		return(RBIODB.TXT)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	EnzymeConn$methods( getEntryContent = function(type, id) {

		if (type == RBIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(id))

			# Request
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.entry.url(RBIODB.ENZYME, accession = x, content.type = RBIODB.TXT)), FUN.VALUE = '')

			return(content)
		}

		return(NULL)
	})

	################
	# CREATE ENTRY #
	################
	
	EnzymeConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == RBIODB.COMPOUND) createEnzymeCompoundFromTxt(content, drop = drop) else NULL)
	})
}
