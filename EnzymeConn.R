if ( ! exists('EnzymeConn')) { # Do not load again if already loaded

	source('RemotedbConn.R')
	source('EnzymeEntry.R')

	#####################
	# CLASS DECLARATION #
	#####################

	EnzymeConn <- setRefClass("EnzymeConn", contains = "RemotedbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	EnzymeConn$methods( getEntryContentType = function() {
		return(BIODB.TXT)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	EnzymeConn$methods( getEntryContent = function(id) {

		# Initialize return values
		content <- rep(NA_character_, length(id))

		# Request
		content <- vapply(id, function(x) .self$.scheduler$getUrl(get.entry.url(BIODB.ENZYME, accession = x, content.type = BIODB.TXT)), FUN.VALUE = '')

		return(content)
	})

	################
	# CREATE ENTRY #
	################
	
	EnzymeConn$methods( createEntry = function(content, drop = TRUE) {
		return(createEnzymeEntryFromTxt(content, drop = drop))
	})
}
