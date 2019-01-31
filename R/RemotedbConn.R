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
#' @export RemotedbConn
#' @exportClass RemotedbConn
RemotedbConn <- methods::setRefClass("RemotedbConn", contains = "BiodbConn")

# Constructor {{{1
################################################################

RemotedbConn$methods( initialize = function(...) {

	callSuper(...)
	.self$.abstract.class('RemotedbConn')

    # Register with request scheduler
	.self$getBiodb()$getRequestScheduler()$.registerConnector(.self)
})

# Get entry content {{{1
################################################################

RemotedbConn$methods( getEntryContent = function(entry.id) {
	# Default implementation
	return(.self$.doGetEntryContentOneByOne(entry.id))
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
		.self$message('debug', "Getting full URL.")
		full.url <- .self$.doGetEntryContentUrl(entry.id, concatenate = concatenate)

		# No single URL for multiple IDs
		if (length(entry.id) > 1 && length(full.url) > 1) {
			.self$message('debug', "Obtained more than one URL.")
#  XXX We must comment out this test, because for PeakforestMass we must return two URLs for each ID (one for LCMS request, and one for LCMSMS request).
#			if (length(full.url) != length(ids))
#				.self$message('error', paste(".doGetEntryContentUrl() does not concatenate IDs to form a single URL. However it returns only ", length(full.url), " URLs for ", length(ids), " IDs. It should return the same number of URLs than IDs.", sep = ''))
			
			urls <- full.url
		}

		else if (max.length == 0 || nchar(full.url) <= max.length) {
			.self$message('debug', paste("Keep single full URL \"", full.url, "\".", sep = ''))
			urls <- full.url
		}

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

# Private methods {{{1
################################################################

# Set request scheduler rules {{{2
################################################################

RemotedbConn$methods( .setRequestSchedulerRules = function() {
})

# Do get entry content url {{{2
################################################################

RemotedbConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {
	.self$.abstract.method()
})

# Do get entry content one by one {{{2
################################################################

RemotedbConn$methods( .doGetEntryContentOneByOne = function(entry.id) {

	# Initialize return values
	content <- rep(NA_character_, length(entry.id))

	# Get URLs
	urls <- .self$getEntryContentUrl(entry.id, concatenate = FALSE)
	
	# Get encoding
	encoding <- .self$getPropertyValue('entry.content.encoding')

	# Send requests
	for (i in seq_along(urls)) {
		lapply(.self$getBiodb()$getObservers(), function(x) x$progress(type = 'info', msg = 'Getting entry contents.', i, length(urls)))
		content[[i]] <- .self$getBiodb()$getRequestScheduler()$getUrl(urls[[i]], encoding = encoding)
	}

	return(content)
})

# Terminate {{{2
################################################################

RemotedbConn$methods( .terminate = function() {

    # Unregister from the request scheduler
	.self$getBiodb()$getRequestScheduler()$.unregisterConnector(.self)
})

# Deprecated methods {{{1
################################################################

# Get URL scheduler {{{2
################################################################

RemotedbConn$methods( .getUrlScheduler = function() {
	.self$.deprecated.method(new.method = "getBiodb()$getRequestScheduler()")
	return(.self$getBiodb()$getRequestScheduler())
})

# Get url {{{2
################################################################

RemotedbConn$methods( .get.url = function(url) {
	.self$.deprecated.method(new.method = "getBiodb()$getRequestScheduler()$getUrl()")
	return(.self$getBiodb()$getRequestScheduler()$getUrl(url))
})
