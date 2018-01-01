# vi: fdm=marker

# Class declaration {{{1
################################################################

#' The connector class to Expasy Enzyme database.
#'
#' This is a concrete connector class. It must never be instantiated directly, but instead be instantiated through the factory \code{\link{BiodbFactory}}. Only specific methods are described here. See super classes for the description of inherited methods.
#'
#' @param name      The name to search for.
#' @param comment   The comment to search for.
#' @param biodb.ids If set to \code{TRUE}, the method will return the entry IDs as a vector of characters, instead of the raw result page.
#'
#' @include CompounddbConn.R
#' @include RemotedbConn.R
#' @export ExpasyEnzymeConn
#' @exportClass ExpasyEnzymeConn
ExpasyEnzymeConn <- methods::setRefClass("ExpasyEnzymeConn", contains = c("RemotedbConn", "CompounddbConn"))

# Get entry content {{{1
################################################################

ExpasyEnzymeConn$methods( getEntryContent = function(entry.id) {

	# Initialize return values
	content <- rep(NA_character_, length(entry.id))

	# Get URLs
	urls <- .self$getEntryContentUrl(entry.id)

	# Request
	content <- vapply(urls, function(url) .self$.getUrlScheduler()$getUrl(url), FUN.VALUE = '')

	return(content)
})

# Web service enzyme-byname {{{1
################################################################

ExpasyEnzymeConn$methods( ws.enzymeByName = function(name, biodb.ids = FALSE) {
	":\n\nCalls enzyme-byname web service and returns the HTML result. See http://enzyme.expasy.org/enzyme-byname.html."

	# Send request
	html.results <- .self$.getUrlScheduler()$getUrl(paste(.self$getBaseUrl(), "enzyme-byname.html", sep = ''), params = name)

	# Parse biodb IDs
	if (biodb.ids) {

		ids <- .self$.parseWsReturnedHtml(html.results)

		return(ids)
	}

	return(html.results)
})

# Web service enzyme-bycomment {{{1
################################################################

ExpasyEnzymeConn$methods( ws.enzymeByComment = function(comment, biodb.ids = FALSE) {
	":\n\nCalls enzyme-bycomment web service and returns the HTML result. See http://enzyme.expasy.org/enzyme-bycomment.html."

	# Send request
	html.results <- .self$.getUrlScheduler()$getUrl(paste(.self$getBaseUrl(), "enzyme-bycomment.html", sep = ''), params = comment)

	# Parse biodb IDs
	if (biodb.ids) {

		ids <- .self$.parseWsReturnedHtml(html.results)

		return(ids)
	}

	return(html.results)
})

# Get entry ids {{{1
################################################################

ExpasyEnzymeConn$methods( getEntryIds = function(max.results = NA_integer_) {

	# Send request
	ids <- .self$ws.enzymeByComment('e', biodb.ids = TRUE)

	# Cut results
	if ( ! is.na(max.results) && length(ids) > max.results)
		ids <- ids[1:max.results]

	return(ids)
})

# PRIVATE METHODS {{{1
################################################################

# Do get entry content url {{{2
################################################################

ExpasyEnzymeConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {

	url <- paste0(.self$getBaseUrl(), 'EC/', id, '.txt')

	return(url)
})

# Parse HTML returned by web services {{{2
################################################################

ExpasyEnzymeConn$methods( .parseWsReturnedHtml = function(html.results) {

	# Parse HTML
	xml <-  XML::htmlTreeParse(html.results, asText = TRUE, useInternalNodes = TRUE)

	# Get ids
	ids <- XML::xpathSApply(xml, "//a[starts-with(@href,'/EC/')]", XML::xmlValue)

	return(ids)
})
