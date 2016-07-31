if ( ! exists('HmdbConn')) { # Do not load again if already loaded

	source('RemotedbConn.R')
	source('HmdbCompound.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	HmdbConn <- setRefClass("HmdbConn", contains = "RemotedbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	HmdbConn$methods( getEntryContentType = function(type) {
		return(BIODB.XML)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	HmdbConn$methods( getEntryContent = function(type, id) {

		if (type == BIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(id))

			# Request
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.entry.url(BIODB.HMDB, x, content.type = BIODB.XML)), FUN.VALUE = '')

			return(content)
		}

		return(NULL)
	})
	
	################
	# CREATE ENTRY #
	################
	
	HmdbConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == BIODB.COMPOUND) createHmdbCompoundFromXml(content, drop = drop) else NULL)
	})
	
} # end of load safe guard
