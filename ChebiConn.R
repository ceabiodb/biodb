if ( ! exists('ChebiConn')) {

	source('RemotedbConn.R')
	source('ChebiEntry.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	ChebiConn <- setRefClass("ChebiConn", contains = "RemotedbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	ChebiConn$methods( getEntryContentType = function() {
		return(BIODB.HTML)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################

	ChebiConn$methods( getEntryContent = function(id) {

		# Initialize return values
		content <- rep(NA_character_, length(id))

		# Request
		content <- vapply(id, function(x) .self$.scheduler$getUrl(get.entry.url(BIODB.CHEBI, x)), FUN.VALUE = '')

		return(content)
	})
	
	################
	# CREATE ENTRY #
	################
	
	ChebiConn$methods( createEntry = function(content, drop = TRUE) {
		return(createChebiEntryFromHtml(content, drop = drop))
	})

}
