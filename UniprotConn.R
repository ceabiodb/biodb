if ( ! exists('UniprotConn')) { # Do not load again if already loaded

	source('RemotedbConn.R')
	source('UniprotEntry.R')

	#####################
	# CLASS DECLARATION #
	#####################

	UniprotConn <- setRefClass("UniprotConn", contains = "RemotedbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	UniprotConn$methods( getEntryContentType = function() {
		return(BIODB.XML)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	UniprotConn$methods( getEntryContent = function(ids) {

		# Initialize return values
		content <- rep(NA_character_, length(ids))

		# Request
		content <- vapply(ids, function(x) .self$.get.url(get.entry.url(BIODB.UNIPROT, x, content.type = BIODB.XML)), FUN.VALUE = '')

		return(content)
	})
	
	################
	# CREATE ENTRY #
	################
	
	UniprotConn$methods( createEntry = function(content, drop = TRUE) {
		return(createUniprotEntryFromXml(content, drop = drop))
	})
}
