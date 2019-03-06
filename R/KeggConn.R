# vi: fdm=marker

# Class declaration {{{1
################################################################

#' The connector abstract class to KEGG databases.
#'
#' This is the mother class of all KEGG connectors. It defines code common to all KEGG connectors.
#'
#' @param query The query to send to the database web service.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{RemotedbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector to a KEGG database
#' conn <- mybiodb$getFactory()$createConn('kegg.compound')
#' 
#' # Search for an entry
#' conn$ws.find('NADPH', retfmt = 'parsed')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include RemotedbConn.R
#' @export KeggConn
#' @exportClass KeggConn
KeggConn <- methods::setRefClass("KeggConn", contains = "RemotedbConn", fields = list(.db.name = "character", .db.abbrev = "character"))

# Constructor {{{1
################################################################

KeggConn$methods( initialize = function(db.name = NA_character_, db.abbrev = NA_character_, ...) {

	callSuper(...)
	.self$.abstract.class('KeggConn')

	# Set name
	if (is.null(db.name) || is.na(db.name))
		.self$message('error', "You must set a name for this KEGG database.")
	.db.name <<- db.name

	# Set abbreviation
	if (is.null(db.abbrev) || is.na(db.abbrev))
		.self$message('error', "You must set an abbreviation for this KEGG database.")
	.db.abbrev <<- db.abbrev
})

# Complete entry id {{{1
################################################################

KeggConn$methods( .complete.entry.id = function(id) {
	return(paste(.self$.db.abbrev, id, sep = ':'))
})

# Get entry content request {{{1
################################################################

KeggConn$methods( .doGetEntryContentRequest = function(id, concatenate = TRUE) {
	return(vapply(id, function(x) BiodbUrl(url = c(.self$getUrl('ws.url'), 'get', x))$toString(), FUN.VALUE = ''))
})

# Get entry page url {{{1
################################################################

KeggConn$methods( getEntryPageUrl = function(id) {
	return(vapply(id, function(x) BiodbUrl(url = c(.self$getUrl('entry.page.url'), 'www_bget'), params = .self$.complete.entry.id(id))$toString(), FUN.VALUE = ''))
})

# Web service list {{{1
################################################################

KeggConn$methods( ws.list = function(retfmt = c('plain', 'request', 'ids')) {
	":\n\nGet lis of entry IDs. See http://www.kegg.jp/kegg/docs/keggapi.html for details."

	retfmt = match.arg(retfmt)

	# Build request
	url <- BiodbUrl(url = c(.self$getUrl('ws.url'), 'list', .self$.db.name))
	request <- BiodbRequest(url = url)
	if (retfmt == 'request')
		return(request)

	# Send request
	results = .self$getBiodb()$getRequestScheduler()$sendRequest(request)

	# Extract IDs
	if (retfmt == 'ids') {
		results <- strsplit(results, "\n")[[1]]
		results <- sub('^[^:]+:([^\\s]+)\\s.*$', '\\1', results, perl = TRUE)
	}

	return(results)
})

# Web service find {{{1
################################################################

KeggConn$methods( ws.find = function(query, retfmt = c('plain', 'request', 'parsed', 'ids')) {
	":\n\nSearch for entries. See http://www.kegg.jp/kegg/docs/keggapi.html for details."

	retfmt = match.arg(retfmt)

	# Build request
	url = BiodbUrl(url = c(.self$getUrl('ws.url'), 'find', .self$.db.name, query))
	request <- BiodbRequest(url = url)
	if (retfmt == 'request')
		return(request)

	# Send request
	results = .self$getBiodb()$getRequestScheduler()$sendRequest(request)

	# Parse
	if (retfmt != 'plain') {

		# Parse data frame
		readtc <- textConnection(results, "r", local = TRUE)
		df <- read.table(readtc, sep = "\t", quote = '', stringsAsFactors = FALSE)
		close(readtc)
		results <- df

		if (retfmt == 'ids')
			results <- results[[1]]
	}

	return(results)
})

# Private methods {{{1
################################################################

# Get entry ids {{{2
################################################################

KeggConn$methods( .doGetEntryIds = function(max.results = NA_integer_) {

	# Get IDs
	ids <- .self$ws.list(retfmt = 'ids')

	return(ids)
})

