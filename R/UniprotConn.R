# vi: fdm=marker

# Class declaration {{{1
################################################################

#' The connector class to Uniprot database.
#'
#' This is a concrete connector class. It must never be instantiated directly, but instead be instantiated through the factory \code{\link{BiodbFactory}}. Only specific methods are described here. See super classes for the description of inherited methods.
#'
#' @param query     The query to send to the database.
#' @param columns   The field columns to retrieve from the database.
#' @param format    The return format.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{RemotedbConn}}, \code{\link{CompounddbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get Uniprot connector
#' uniprot <- mybiodb$getFactory()$createConn('uniprot')
#'
#' # Access web service query
#' result <- uniprot$ws.query(query = 'name:"prion protein"', columns = c('id', 'entry name'), format = 'txt', limit = 10)
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

UniprotConn$methods( ws.query = function(query = '', columns = NA, format = NA, limit = NA) {
	"Direct query to the database for searching for compounds. See http://www.uniprot.org/help/api_queries for details."

	params = list()

	# Set URL
	url <- .self$getBaseUrl()

	# Set other parameters
	params[['query']] <- query
	if ( ! is.null(columns) && ! is.na(columns))
		params[['columns']] <- paste(columns, collapse = ',')
	if ( ! is.null(format) && ! is.na(format))
		params[['format']] <- format
	if ( ! is.null(limit) && ! is.na(limit))
		params[['limit']] <- limit

	result <- .self$.getUrlScheduler()$getUrl(url, params = params)

	return(result)
})

# Web service query IDs {{{1
################################################################

UniprotConn$methods( ws.query.ids = function(...) {
	"Calls ws.query() but only for getting IDs. Returns the IDs as a character vector."

	results <- .self$ws.query(columns = 'id', format = 'tab', ...)
	readtc <- textConnection(results, "r", local = TRUE)
	df <- read.table(readtc, sep = "\t", header = TRUE)
	ids <- as.character(df[[1]])

	return(ids)
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

	ids <- .self$ws.query.ids(limit = max.results)

	return(ids)
})

# Do get entry content url {{{1
################################################################

UniprotConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {
	                    
	url <- paste0(.self$getBaseUrl(), id, '.xml')

	return(url)
})

# Search compound {{{1
################################################################

UniprotConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {

	query <- ''

	# Search for name
	if ( ! is.null(name) && ! is.na(name)) {
		name.query <- paste('name', paste0('"', name, '"'), sep = ':')
		mnemonic.query <- paste('mnemonic', paste0('"', name, '"'), sep = ':')
		query <- paste(name.query, mnemonic.query, sep = ' OR ')
	}

	# Search for mass
	if ( ! is.null(mass) && ! is.na(mass)) {

		if (mass.tol.unit == 'ppm') {
			mass.min <- mass * (1 - mass.tol * 1e-6)
			mass.max <- mass * (1 + mass.tol * 1e-6)
		} else {
			mass.min <- mass - mass.tol
			mass.max <- mass + mass.tol
		}

 		# Uniprot does not accept mass in floating numbers
		mass.min <- as.integer(mass.min)
		mass.max <- as.integer(mass.max)

		mass.query <- paste0('mass:[', mass.min, ' TO ', mass.max, ']')

		if (nchar(query) > 0) {
			query <- paste0('(', query, ')')
			query <- paste(query, mass.query, sep = ' AND ')
		}
		else
			query <- mass.query
	}

	# Send query
	ids <- .self$ws.query.ids(query = query, limit = max.results)

	return(ids)
})
