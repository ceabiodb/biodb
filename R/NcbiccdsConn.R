# vi: fdm=marker

# Class declaration {{{1
################################################################

NcbiCcdsConn <- methods::setRefClass("NcbiCcdsConn", contains = "NcbiConn")

# Constructor {{{1
################################################################

NcbiCcdsConn$methods( initialize = function(...) {

	# Call parent constructor
	callSuper(base.url = 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/', content.type = BIODB.HTML, ...)
})

# Get entry content {{{1
################################################################

NcbiCcdsConn$methods( getEntryContent = function(id) {

	# Initialize return values
	content <- rep(NA_character_, length(id))

	# Request
	content <- vapply(id, function(x) .self$.get.url(get.entry.url(BIODB.NCBICCDS, x, content.type = BIODB.HTML)), FUN.VALUE = '')

	return(content)
})

# Create entry {{{1
################################################################

NcbiCcdsConn$methods( createEntry = function(content, drop = TRUE) {
	return(createNcbiCcdsEntryFromHtml(.self$getBiodb(), content, drop = drop))
})

# Get entry ids {{{1
################################################################

NcbiCcdsConn$methods( getEntryIds = function(max.results = NA_integer_) {
	return(NULL)
})
