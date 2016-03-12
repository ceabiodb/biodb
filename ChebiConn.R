if ( ! exists('ChebiConn')) { # Do not load again if already loaded

	source('BiodbConn.R')
	source('ChebiCompound.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	ChebiConn <- setRefClass("ChebiConn", contains = "BiodbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	ChebiConn$methods( getEntryContentType = function(type) {
		return(RBIODB.HTML)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	ChebiConn$methods( getEntryContent = function(type, id) {

		if (type == RBIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(id))

			# Request
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.chebi.compound.url(x)), FUN.VALUE = '')

			return(content)
		}

		return(NULL)
	})
	
	################
	# CREATE ENTRY #
	################
	
	ChebiConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == RBIODB.COMPOUND) createChebiCompoundFromHtml(content, drop = drop) else NULL)
	})

	##########################
	# GET CHEBI COMPOUND URL #
	##########################
	
	get.chebi.compound.url <- function(id) {
	
		url <- paste0('https://www.ebi.ac.uk/chebi/searchId.do?chebiId=', id)
	
		return(url)
	}

} # end of load safe guard
