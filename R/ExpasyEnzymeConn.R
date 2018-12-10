# vi: fdm=marker

# Constants {{{1
################################################################

.BIODB.EXPAZY.ENZYME.PARSING.EXPR <- list(
	'accession'             = "^ID\\s+([0-9.]+)$",
	'name'                  = "^DE\\s+(.+?)\\.?$",
	'catalytic.activity'    = "^CA\\s+(.+?)\\.?$",
	'cofactor'              = "^CF\\s+(.+?)\\.?$"
)

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

# Search compound {{{1
################################################################

ExpasyEnzymeConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.field = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {
		
	.self$.checkMassField(mass = mass, mass.field = mass.field)

	ids <- NULL

	# Search for name
	if ( ! is.null(name))
		ids <- .self$ws.enzymeByName(name, biodb.ids = TRUE)

	# Search by mass
	if ( ! is.null(mass.field))
		.self$message('caution', paste0('Mass search is not handled.'))

	return(ids)
})

# Get entry page url {{{1
################################################################

ExpasyEnzymeConn$methods( getEntryPageUrl = function(id) {
	return(paste0(.self$getBaseUrl(), 'cgi-bin/enzyme/enzyme-search-ec?', sub('^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)$', 'field1=\\1&field2=\\2&field3=\\3&field4=\\4', id)))
})

# Get entry image url {{{1
################################################################

ExpasyEnzymeConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})

# Private methods {{{1
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

# Get parsing expressions {{{2
################################################################

ExpasyEnzymeConn$methods( .getParsingExpressions = function() {
	return(.BIODB.EXPAZY.ENZYME.PARSING.EXPR)
})
