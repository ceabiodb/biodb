# vi: fdm=marker

# Class declaration {{{1
################################################################

#' The connector abstract class to KEGG databases.
#'
#' This is the mother class of all KEGG connectors. It defines code common to all KEGG connectors.
#'
#' @param query The query to send to the database web service.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{BiodbRemotedbConn}}, \code{\link{BiodbSearchable}}.
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
#' @include BiodbRemotedbConn.R
#' @include BiodbSearchable.R
#' @export KeggConn
#' @exportClass KeggConn
KeggConn <- methods::setRefClass("KeggConn", contains = c("BiodbRemotedbConn", "BiodbSearchable"), fields = list(.db.name = "character", .db.abbrev = "character"))

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
	.db.abbrev <<- db.abbrev
})

# Complete entry id {{{1
################################################################

KeggConn$methods( .complete.entry.id = function(id) {

	if ( ! is.na(.self$.db.abbrev) && nchar(.self$.db.abbrev) > 0)
		id = paste(.self$.db.abbrev, id, sep = ':')

	return(id)
})

# Get entry content request {{{1
################################################################

KeggConn$methods( .doGetEntryContentRequest = function(id, concatenate = TRUE) {
	return(vapply(id, function(x) BiodbUrl(url = c(.self$getPropValSlot('urls', 'ws.url'), 'get', x))$toString(), FUN.VALUE = ''))
})

# Get entry page url {{{1
################################################################

KeggConn$methods( getEntryPageUrl = function(id) {
	return(vapply(id, function(x) BiodbUrl(url = c(.self$getPropValSlot('urls', 'entry.page.url'), 'www_bget'), params = .self$.complete.entry.id(id))$toString(), FUN.VALUE = ''))
})

# Web service list {{{1
################################################################

KeggConn$methods( ws.list = function(retfmt = c('plain', 'request', 'ids')) {
	":\n\nGet list of entry IDs. See http://www.kegg.jp/kegg/docs/keggapi.html for details."

	retfmt = match.arg(retfmt)

	# Build request
	url <- BiodbUrl(url = c(.self$getPropValSlot('urls', 'ws.url'), 'list', .self$.db.name))
	request <- BiodbRequest(url = url)
	if (retfmt == 'request')
		return(request)

	# Send request
	results = .self$getBiodb()$getRequestScheduler()$sendRequest(request)

	# Extract IDs
	if (retfmt == 'ids') {
		results = strsplit(results, "\n")[[1]]
		
		if ( ! is.na(.self$.db.abbrev) && nchar(.self$.db.abbrev) > 0)
			results = sub('^[^:]+:([^\\s]+)\\s.*$', '\\1', results, perl = TRUE)
		else
			results = sub('^([^\\s]+)\\s.*$', '\\1', results, perl = TRUE)
	}

	return(results)
})

# Web service find {{{1
################################################################

KeggConn$methods( ws.find = function(query, retfmt = c('plain', 'request', 'parsed', 'ids')) {
	":\n\nSearch for entries. See http://www.kegg.jp/kegg/docs/keggapi.html for details."

	retfmt = match.arg(retfmt)

	# Build request
	url = BiodbUrl(url = c(.self$getPropValSlot('urls', 'ws.url'), 'find', .self$.db.name, query))
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

# Search by name {{{1
################################################################

KeggConn$methods( searchByName = function(name, max.results = NA_integer_) {

	ids <- NULL

	# Search by name
	if ( ! is.null(name) && ! is.na(name)) {
		ids <- .self$ws.find(name, retfmt = 'ids')
		if ( ! is.na(.self$.db.abbrev) && nchar(.self$.db.abbrev) > 0)
			ids <- sub('^[^:]*:', '', ids)
	}

	# Cut
	if ( ! is.na(max.results) && max.results > 0 && max.results < length(ids))
		ids <- ids[1:max.results]

	return(ids)
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

