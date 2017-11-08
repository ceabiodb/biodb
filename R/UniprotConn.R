# vi: fdm=marker

# Class declaration {{{1
################################################################

#' The connector class to Uniprot database.
#'
#' This is a concrete connector class. It must never be instantiated directly, but instead be instantiated through the factory \code{\link{BiodbFactory}}. Only specific methods are described here. See super classes for the description of inherited methods.
#'
#' @param columns   The field columns to retrieve from the database.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{RemotedbConn}}, \code{\link{CompounddbConn}}.
#'
#' @include CompounddbConn.R
#' @include RemotedbConn.R
#' @export UniprotConn
#' @exportClass UniprotConn
UniprotConn <- methods::setRefClass("UniprotConn", contains = c("RemotedbConn", "CompounddbConn"))

# Constructor {{{1
################################################################

UniprotConn$methods( initialize = function(...) {
	callSuper(...)
})

# Web service query {{{1
################################################################

UniprotConn$methods( ws.query = function(columns = NA, format = NA, limit = NA) {
	"Direct query to the database for searching for queries. See http://www.uniprot.org/help/api_queries for details."

	params = list()

	# Set URL
	url <- .self$getBaseUrl()

	# Set query parameter
	params[['query']] <- ''

	# Set other parameters
	if ( ! is.na(columns))
		params[['columns']] <- columns
	if ( ! is.na(format))
		params[['format']] <- format
	if ( ! is.na(limit))
		params[['limit']] <- limit

	result <- .self$.getUrlScheduler()$getUrl(url, params = params)

	return(result)
})

# Get entry content {{{1
################################################################

UniprotConn$methods( getEntryContent = function(entry.id) {

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

UniprotConn$methods( getEntryIds = function(max.results = NA_integer_) {
	.self$message('caution', "No method implemented for computing list of IDs.")
	return(NULL)
})

# Do get entry content url {{{1
################################################################

UniprotConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {
	                    
	url <- paste0(.self$getBaseUrl(), id, '.xml')

	return(url)
})
