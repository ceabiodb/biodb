# vi: fdm=marker

# Class declaration {{{1
################################################################

#' The mother class of all remote database connectors.
#'
#' This is the super class of remote database connectors. It thus defines methods related to remote connection, like the definition of a token, and URL definitions. As with \code{\link{BiodbConn}} class, you won't need to use the constructor. Nevertheless we provide in the Fields section information about the constructor parameters, for eventual developers.
#'
#' @param concatenate   If set to \code{TRUE}, then try to build as few URLs as possible, sending requests with several identifiers at once.
#' @param entry.id      The identifiers (e.g.: accession numbers) as a \code{character vector} of the database entries.
#' @param max.length    The maximum length of the URLs to return, in number of characters.
#'
#' @seealso \code{\link{BiodbConn}}, \code{\link{BiodbRequestScheduler}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get connector
#' conn <- mybiodb$getFactory()$createConn('chemspider')
#'
#' # Get the picture URL of an entry
#' picture.url <- conn$getEntryImageUrl('2')
#'
#' # Get the page URL of an entry
#' page.url <- conn$getEntryPageUrl('2')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @include BiodbConn.R
#' @include BiodbRequestScheduler.R
#' @export BiodbRemotedbConn
#' @exportClass BiodbRemotedbConn
BiodbRemotedbConn <- methods::setRefClass("BiodbRemotedbConn", contains = "BiodbConn")

# Constructor {{{1
################################################################

BiodbRemotedbConn$methods( initialize = function(...) {

	callSuper(...)
	.self$.abstract.class('BiodbRemotedbConn')

    # Register with request scheduler
	.self$getBiodb()$getRequestScheduler()$.registerConnector(.self)
})

# Get entry content from database {{{1
################################################################

BiodbRemotedbConn$methods( getEntryContentFromDb = function(entry.id) {
	# Default implementation
	return(.self$.doGetEntryContentOneByOne(entry.id))
})

# Get entry content request {{{1
################################################################

BiodbRemotedbConn$methods( getEntryContentRequest = function(entry.id, concatenate = TRUE, max.length = 0) {
	":\n\nGet the URL to use in order to get the contents of the specified entries."

	# Copy code from get.entry.url
	# 

	urls <- character(0)

	if (length(entry.id) > 0) {

		# Get full URL
		full.url <- .self$.doGetEntryContentRequest(entry.id, concatenate = concatenate)

		# No single URL for multiple IDs
		if ((length(entry.id) > 1 && length(full.url) > 1) || max.length == 0 || nchar(full.url) <= max.length)
			urls <- full.url

		# full.url is too big, we must split it
		else {
			.self$message('debug', "Split full URL.")

			start <- 1

			# Loop as long as there are IDs
			while (start <= length(entry.id)) {
				# Find max size URL
				a <- start
				b <- length(entry.id)
				while (a < b) {
					m <- as.integer((a + b) / 2)
					url <- .self$.doGetEntryContentRequest(entry.id[start:m])
					if (all(nchar(url) <= max.length) && m != a)
						a <- m
					else
						b <- m
				}
				urls <- c(urls, .self$.doGetEntryContentRequest(entry.id[start:a]))
				start <- a + 1
			}
		}
	}

	return(urls)
})

# Get entry image url {{{1
################################################################

BiodbRemotedbConn$methods( getEntryImageUrl = function(entry.id) {
	":\n\nGet the URL to a picture of the entry (e.g.: a picture of the molecule in case of a compound entry)."

	.self$.abstract.method()
})

# Get entry page url {{{1
################################################################

BiodbRemotedbConn$methods( getEntryPageUrl = function(entry.id) {
	":\n\nGet the URL to the page of the entry on the database web site."

	.self$.abstract.method()
})

# Private methods {{{1
################################################################

# Set request scheduler rules {{{2
################################################################

BiodbRemotedbConn$methods( .setRequestSchedulerRules = function() {
})

# Do get entry content request {{{2
################################################################

BiodbRemotedbConn$methods( .doGetEntryContentRequest = function(id, concatenate = TRUE) {
	.self$.abstract.method()
})

# Do get entry content one by one {{{2
################################################################

BiodbRemotedbConn$methods( .doGetEntryContentOneByOne = function(entry.id) {

	# Initialize return values
	content <- rep(NA_character_, length(entry.id))

	# Get requests
	requests <- .self$getEntryContentRequest(entry.id, concatenate = FALSE)
	
	# Get encoding
	encoding <- .self$getPropertyValue('entry.content.encoding')

	# If requests is a vector of characters, then the method is using the old scheme.
	# We now convert the requests to the new scheme, using class BiodbRequest.
	if (is.character(requests))
		requests <- lapply(requests, function(x) BiodbRequest(method = 'get', url = BiodbUrl(x), encoding = encoding))

	# Send requests
	for (i in seq_along(requests)) {
		lapply(.self$getBiodb()$getObservers(), function(x) x$progress(type = 'info', msg = 'Getting entry contents.', index = i, total = length(requests), first = (i == 1)))
		content[[i]] <- .self$getBiodb()$getRequestScheduler()$sendRequest(requests[[i]])
	}

	return(content)
})

# Terminate {{{2
################################################################

BiodbRemotedbConn$methods( .terminate = function() {

    # Unregister from the request scheduler
	.self$getBiodb()$getRequestScheduler()$.unregisterConnector(.self)
})

