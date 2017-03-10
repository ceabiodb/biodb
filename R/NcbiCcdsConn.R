# vi: fdm=marker

#' @include NcbiConn.R

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

	# Get URLs
	urls <- .self$getEntryContentUrl(id)

	# Request
	content <- vapply(urls, function(url) .self$.getUrlScheduler()$getUrl(url), FUN.VALUE = '')

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

# Do get entry content url {{{1
################################################################

NcbiCcdsConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {
	return(.self$getEntryPageUrl(id))
})

# Get entry page url {{{1
################################################################

NcbiCcdsConn$methods( getEntryPageUrl = function(id) {

	url <- paste0('https://www.ncbi.nlm.nih.gov/CCDS/CcdsBrowse.cgi?REQUEST=CCDS&GO=MainBrowse&DATA=', id)

	return(url)
})
