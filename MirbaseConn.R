if ( ! exists('MirbaseConn')) { # Do not load again if already loaded

	source('BiodbConn.R')
	source('MirbaseCompound.R')

	#####################
	# CLASS DECLARATION #
	#####################

	MirbaseConn <- setRefClass("MirbaseConn", contains = "BiodbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	MirbaseConn$methods( getEntryContentType = function(type) {
		return(RBIODB.HTML)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	MirbaseConn$methods( getEntryContent = function(type, id) {

		if (type == RBIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(id))

			# Request
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.entry.url(RBIODB.MIRBASE, x, content.type = RBIODB.HTML)), FUN.VALUE = '')

			return(content)
		}

		return(NULL)
	})

	################
	# CREATE ENTRY #
	################
	
	MirbaseConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == RBIODB.COMPOUND) createMirbaseCompoundFromHtml(content, drop = drop) else NULL)
	})

	###################
	# FIND ACCESSIONS #
	###################

	MirbaseConn$methods(
		findAccessions = function(name) {

			# Get HTML
			htmlstr <- .self$.scheduler$getUrl('http://www.mirbase.org/cgi-bin/query.pl', params = c(terms = name, submit = 'Search'))

			# Parse HTML
			xml <-  htmlTreeParse(htmlstr, asText = TRUE, useInternalNodes = TRUE)

			# Get accession number
			acc <- unlist(xpathSApply(xml, "//a[starts-with(.,'MIMAT')]", xmlValue))

			return(acc)
	})
}
