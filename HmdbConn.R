if ( ! exists('HmdbConn')) { # Do not load again if already loaded

	source('BiodbConn.R')
	source('HmdbCompound.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	HmdbConn <- setRefClass("HmdbConn", contains = "BiodbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	HmdbConn$methods( getEntryContentType = function(type) {
		return(RBIODB.XML)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	HmdbConn$methods( getEntryContent = function(type, id) {

		if (type == RBIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(id))

			# Request
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.entry.url(RBIODB.HMDB, x, content.type = RBIODB.XML)), FUN.VALUE = '')

			return(content)
		}

		return(NULL)
	})
	
	################
	# CREATE ENTRY #
	################
	
	HmdbConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == RBIODB.COMPOUND) createHmdbCompoundFromXml(content, drop = drop) else NULL)
	})
	
} # end of load safe guard
