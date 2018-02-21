# vi: fdm=marker

#' @include NcbiConn.R

# Class declaration {{{1
################################################################

NcbiCcdsConn <- methods::setRefClass("NcbiCcdsConn", contains = "NcbiConn")

# Constructor {{{1
################################################################

NcbiCcdsConn$methods( initialize = function(...) {

	# Call parent constructor
	callSuper(...)
})

# Get entry content {{{1
################################################################

NcbiCcdsConn$methods( getEntryContent = function(entry.id) {

	# Initialize return values
	content <- rep(NA_character_, length(entry.id))

	# Get URLs
	urls <- .self$getEntryContentUrl(entry.id)

	# Request
	content <- vapply(urls, function(url) .self$.getUrlScheduler()$getUrl(url), FUN.VALUE = '')

	return(content)
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
	return(paste0(file.path(.self$getBaseUrl(), 'CcdsBrowse.cgi', fsep = '/'), '?REQUEST=CCDS&GO=MainBrowse&DATA=', id))
})

# Get entry image url {{{1
################################################################

NcbiCcdsConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})
