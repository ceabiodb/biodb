if ( ! exists('MirbaseConn')) { # Do not load again if already loaded

	source('RemotedbConn.R')
	source('MirbaseEntry.R')

	#####################
	# CLASS DECLARATION #
	#####################

	MirbaseConn <- setRefClass("MirbaseConn", contains = "RemotedbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	MirbaseConn$methods( getEntryContentType = function() {
		return(BIODB.HTML)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	MirbaseConn$methods( getEntryContent = function() {

		# Initialize return values
		content <- rep(NA_character_, length(id))

		# Request
		content <- vapply(id, function(x) .self$.get.url(get.entry.url(BIODB.MIRBASE, x, content.type = BIODB.HTML)), FUN.VALUE = '')

		return(content)
	})

	################
	# CREATE ENTRY #
	################
	
	MirbaseConn$methods( createEntry = function(content, drop = TRUE) {
		return(createMirbaseEntryFromHtml(content, drop = drop))
	})

	###################
	# FIND ACCESSIONS #
	###################

	MirbaseConn$methods( findAccessions = function(name) {

		# Get HTML
		htmlstr <- .self$.get.url('http://www.mirbase.org/cgi-bin/query.pl', params = c(terms = name, submit = 'Search'))

		# Parse HTML
		xml <-  htmlTreeParse(htmlstr, asText = TRUE, useInternalNodes = TRUE)

		# Get accession number
		acc <- unlist(xpathSApply(xml, "//a[starts-with(.,'MIMAT')]", xmlValue))

		return(acc)
	})
}
