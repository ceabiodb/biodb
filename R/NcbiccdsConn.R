# vi: fdm=marker

# Class declaration {{{1
################################################################

NcbiccdsConn <- methods::setRefClass("NcbiccdsConn", contains = "NcbiConn")

# Constructor {{{1
################################################################

NcbiccdsConn$methods( initialize = function(...) {

	# Call parent constructor
	callSuper(base.url = 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/', content.type = BIODB.HTML, ...)
})

#####################
# GET ENTRY CONTENT #
#####################

NcbiccdsConn$methods( getEntryContent = function(id) {

	# Initialize return values
	content <- rep(NA_character_, length(id))

	# Request
	content <- vapply(id, function(x) .self$.get.url(get.entry.url(BIODB.NCBICCDS, x, content.type = BIODB.HTML)), FUN.VALUE = '')

	return(content)
})

################
# CREATE ENTRY #
################

NcbiccdsConn$methods( createEntry = function(content, drop = TRUE) {
	return(createNcbiccdsEntryFromHtml(.self$getBiodb(), content, drop = drop))
})
