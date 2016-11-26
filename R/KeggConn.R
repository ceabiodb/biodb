if ( ! exists('KeggConn')) {

	#####################
	# CLASS DECLARATION #
	#####################
	
	KeggConn <- setRefClass("KeggConn", contains = "RemotedbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	KeggConn$methods( getEntryContentType = function() {
		return(BIODB.TXT)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################

	KeggConn$methods( getEntryContent = function(id) {

		# Initialize return values
		content <- rep(NA_character_, length(id))

		# Request
		content <- vapply(id, function(x) .self$.get.url(get.entry.url(BIODB.KEGG, x, content.type = BIODB.TXT)), FUN.VALUE = '')

		return(content)
	})

	################
	# CREATE ENTRY #
	################

	KeggConn$methods( createEntry = function(content, drop = TRUE) {
		return(createKeggEntryFromTxt(content, drop = drop))
	})

}
