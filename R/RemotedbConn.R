# vi: fdm=marker

# Class declaration {{{1
################################################################

#' The mother class of all remote database connectors.
#'
#' This is the super class of remote database connectors. It thus defines methods related to remote connection, like the definition of a token, and URL definitions. As with \code{\link{BiodbConn}} class, you won't need to use the constructor. Nevertheless we provide in the Fields section information about the constructor parameters, for eventual developers.
#'
#' @field scheduler     An instance from the class \code{\link{UrlRequestScheduler.R}}.
#' @field token         An access token as a character string, required by some databases for all or part of their webservices.
#'
#' @param concatenate   If set to \code{TRUE}, then try to build as few URLs as possible, sending requests with several identifiers at once.
#' @param entry.id      The identifiers (e.g.: accession numbers) as a \code{character vector} of the database entries.
#' @param max.length    The maximum length of the URLs to return, in number of characters.
#' @param token         An access token as a character string, required by some databases for all or part of their webservices.
#'
#' @seealso \code{\link{BiodbConn}}, \code{\link{UrlRequestScheduler}}.
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
#' @import methods
#' @include BiodbConn.R
#' @include UrlRequestScheduler.R
#' @export RemotedbConn
#' @exportClass RemotedbConn
RemotedbConn <- methods::setRefClass("RemotedbConn", contains = "BiodbConn", fields = list(.scheduler = "ANY", .token = "character"))

# Constructor {{{1
################################################################

RemotedbConn$methods( initialize = function(scheduler = NULL, token = NA_character_, ...) {

	callSuper(...)

	# Set token
	if (is.na(token))
		token <- .self$getBiodb()$getDbsInfo()$get(.self$getId())$getToken()
	.token <<- token

	# Set scheduler
	if (is.null(scheduler))
		scheduler <- UrlRequestScheduler$new(n = 3, parent = .self)
	if ( ! is(scheduler, "UrlRequestScheduler"))
		.self$message(MSG.ERROR, "The scheduler instance must inherit from UrlRequestScheduler class.")
	.scheduler <<- scheduler
})

# Get token {{{1
################################################################

RemotedbConn$methods( getToken = function() {
	":\n\nGet the token configured for this connector."

	return(.self$.token)
})

# Set token {{{1
################################################################

RemotedbConn$methods( setToken = function(token) {
	":\n\nSet a token for this connector."

	.token <<- token
})

# Get entry content url {{{1
################################################################

RemotedbConn$methods( getEntryContentUrl = function(entry.id, concatenate = TRUE, max.length = 0) {
	":\n\nGet the URL to use in order to get the contents of the specified entries."

	# Copy code from get.entry.url
	# 

	urls <- character(0)

	if (length(entry.id) > 0) {

		# Get full URL
		.self$message(MSG.DEBUG, "Getting full URL.")
		full.url <- .self$.doGetEntryContentUrl(entry.id, concatenate = concatenate)

		# No single URL for multiple IDs
		if (length(entry.id) > 1 && length(full.url) > 1) {
			.self$message(MSG.DEBUG, "Obtained more than one URL.")
#  XXX We must comment out this test, because for PeakforestMass we must return two URLs for each ID (one for LCMS request, and one for LCMSMS request).
#			if (length(full.url) != length(ids))
#				.self$message(MSG.ERROR, paste(".doGetEntryContentUrl() does not concatenate IDs to form a single URL. However it returns only ", length(full.url), " URLs for ", length(ids), " IDs. It should return the same number of URLs than IDs.", sep = ''))
			
			urls <- full.url
		}

		else if (max.length == 0 || nchar(full.url) <= max.length) {
			.self$message(MSG.DEBUG, paste("Keep single full URL \"", full.url, "\".", sep = ''))
			urls <- full.url
		}

		# full.url is too big, we must split it
		else {
			.self$message(MSG.DEBUG, "Split full URL.")

			start <- 1

			# Loop as long as there are IDs
			while (start <= length(entry.id)) {
				# Find max size URL
				a <- start
				b <- length(entry.id)
				while (a < b) {
					m <- as.integer((a + b) / 2)
					url <- .self$.doGetEntryContentUrl(entry.id[start:m])
					if (all(nchar(url) <= max.length) && m != a)
						a <- m
					else
						b <- m
				}
				urls <- c(urls, .self$.doGetEntryContentUrl(entry.id[start:a]))
				start <- a + 1
			}
		}
	}

	return(urls)
})

# Get entry image url {{{1
################################################################

RemotedbConn$methods( getEntryImageUrl = function(entry.id) {
	":\n\nGet the URL to a picture of the entry (e.g.: a picture of the molecule in case of a compound entry)."

	.self$.abstract.method()
})

# Get entry page url {{{1
################################################################

RemotedbConn$methods( getEntryPageUrl = function(entry.id) {
	":\n\nGet the URL to the page of the entry on the database web site."

	.self$.abstract.method()
})

# PRIVATE METHODS {{{1
################################################################

# Get URL scheduler {{{2
################################################################

RemotedbConn$methods( .getUrlScheduler = function() {

	return(.self$.scheduler)
})

# Do get entry content url {{{1
################################################################

RemotedbConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {
	"Get the contents of specified entry identifiers. 
	id: A character vector containing the identifiers.
	return: A character vector containing the entry contents. NULL if no identifier is given (empty vector)."

	.self$.abstract.method()
})

# Get url {{{1
################################################################

RemotedbConn$methods( .get.url = function(url) {
	.self$.deprecated.method(new.method = ".getUrlScheduler()$getUrl()")
	return(.self$.scheduler$getUrl(url))
})

# Set user agent {{{1
################################################################

RemotedbConn$methods( .set.useragent = function(useragent) {
	.scheduler$setUserAgent(useragent) # set agent
})
