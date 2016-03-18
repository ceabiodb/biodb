if ( ! exists('EnzymeConn')) { # Do not load again if already loaded

	source('BiodbConn.R')
	source('EnzymeCompound.R')

	#####################
	# CLASS DECLARATION #
	#####################

	EnzymeConn <- setRefClass("EnzymeConn", contains = "BiodbConn")

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
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.enzyme.compound.url(x)), FUN.VALUE = '')

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
	
	###########################
	# GET ENZYME COMPOUND URL #
	###########################
	
	get.enzyme.compound.url <- function(id) {

		url <- paste0('http://enzyme.expasy.org/EC/', id, '.txt')
	
		return(url)
	}
}
