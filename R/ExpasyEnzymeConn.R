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
#' @include BiodbCompounddbConn.R
#' @include BiodbRemotedbConn.R
#' @export ExpasyEnzymeConn
#' @exportClass ExpasyEnzymeConn
ExpasyEnzymeConn <- methods::setRefClass("ExpasyEnzymeConn", contains = c("BiodbRemotedbConn", "BiodbCompounddbConn"))

# Web service enzyme-byname {{{1
################################################################

ExpasyEnzymeConn$methods( ws.enzymeByName = function(name, retfmt = c('plain', 'request', 'parsed', 'ids')) {
	":\n\nCalls enzyme-byname web service and returns the HTML result. See http://enzyme.expasy.org/enzyme-byname.html."

	retfmt = match.arg(retfmt)

	# Build request
	request = BiodbRequest(method = 'get', url = BiodbUrl(url = c(.self$getUrl('base.url'), "enzyme-byname.html"), params = name))
	if (retfmt == 'request')
		return(request)

	# Send request
	results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

	# Parse HTML
	results <- .self$.parseWsReturnedHtml(results = results, retfmt = retfmt)

	return(results)
})

# Web service enzyme-bycomment {{{1
################################################################

ExpasyEnzymeConn$methods( ws.enzymeByComment = function(comment, retfmt = c('plain', 'request', 'parsed', 'ids')) {
	":\n\nCalls enzyme-bycomment web service and returns the HTML result. See http://enzyme.expasy.org/enzyme-bycomment.html."

	retfmt = match.arg(retfmt)

	# Build request
	request = BiodbRequest(method = 'get', url = BiodbUrl(url = c(.self$getUrl('base.url'), "enzyme-bycomment.html"), params = comment))
	if (retfmt == 'request')
		return(request)

	# Send request
	results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

	# Parse HTML
	results <- .self$.parseWsReturnedHtml(results = results, retfmt = retfmt)

	return(results)
})

# Search compound {{{1
################################################################

ExpasyEnzymeConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.field = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {
		
	.self$.checkMassField(mass = mass, mass.field = mass.field)

	ids <- NULL

	# Search for name
	if ( ! is.null(name))
		ids <- .self$ws.enzymeByName(name, retfmt = 'ids')

	# Search by mass
	if ( ! is.null(mass.field))
		.self$message('caution', paste0('Mass search is not handled.'))

	# Cut
	if ( ! is.na(max.results) && max.results > 0 && max.results < length(ids))
		ids <- ids[1:max.results]

	return(ids)
})

# Get entry page url {{{1
################################################################

ExpasyEnzymeConn$methods( getEntryPageUrl = function(id) {

	urls = rep(NA_character_, length(id))

	# Loop on all IDs
	i = 0
	for (x in id) {

		i = i + 1

		# Get four fields of ID
		fields = strsplit(x, '\\.')[[1]]
		if (length(fields) == 4)
			urls[[i]] = BiodbUrl(url = c(.self$getUrl('base.url'), 'cgi-bin', 'enzyme', 'enzyme-search-ec'), params = list(field1 = fields[[1]], field2 = fields[[2]], field3 = fields[[3]], field4 = fields[[4]]))$toString()
	}

	return(urls)
})

# Get entry image url {{{1
################################################################

ExpasyEnzymeConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})

# Private methods {{{1
################################################################

# Do get entry content request {{{2
################################################################

ExpasyEnzymeConn$methods( .doGetEntryContentRequest = function(id, concatenate = TRUE) {

	urls <- vapply(id, function(x) BiodbUrl(url = c(.self$getUrl('base.url'), 'EC', paste(x, 'txt', sep = '.')))$toString(), FUN.VALUE = '')

	return(urls)
})

# Parse HTML returned by web services {{{2
################################################################

ExpasyEnzymeConn$methods( .parseWsReturnedHtml = function(results, retfmt) {

	if (retfmt %in% c('parsed', 'ids')) {

		# Parse HTML
		results <-  XML::htmlTreeParse(results, asText = TRUE, useInternalNodes = TRUE)

		# Get ids
		if (retfmt == 'ids')
			results <- XML::xpathSApply(results, "//a[starts-with(@href,'/EC/')]", XML::xmlValue)
	}

	return(results)
})

# Get parsing expressions {{{2
################################################################

ExpasyEnzymeConn$methods( .getParsingExpressions = function() {
	return(.BIODB.EXPAZY.ENZYME.PARSING.EXPR)
})

# Get entry ids {{{2
################################################################

ExpasyEnzymeConn$methods( .doGetEntryIds = function(max.results = NA_integer_) {

	# Send request
	ids <- .self$ws.enzymeByComment('e', retfmt = 'ids')

	return(ids)
})

