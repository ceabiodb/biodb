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
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.kegg.compound.url(x, txt = TRUE)), FUN.VALUE = '')

			return(content)
		}

		return(NULL)
	})
	
	################
	# CREATE ENTRY #
	################
	
	KeggConn$methods( createEntry = function(type, content) {
		return(if (type == RBIODB.COMPOUND) createKeggCompoundFromTxt(content) else NULL)
	})
	
	#########################
	# GET KEGG COMPOUND URL #
	#########################
	
	get.kegg.compound.url <- function(id, txt = FALSE) {

		if (txt)
			url <- paste0('http://rest.kegg.jp/get/', id)
		else
			url <- paste0('http://www.genome.jp/dbget-bin/www_bget?cpd:', id)
	
		return(url)
	}

} # end of load safe guard
