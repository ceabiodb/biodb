# vi: fdm=marker

# Class declaration {{{1
################################################################

RemotedbConn <- methods::setRefClass("RemotedbConn", contains = "BiodbConn", fields = list(.scheduler = "UrlRequestScheduler", .token = "character"))

# Constructor {{{1
################################################################

RemotedbConn$methods( initialize = function(scheduler = NULL, token = NA_character_, ...) {

	callSuper(...)

	# Set token
	.token <<- token

	# Set scheduler
	if (is.null(scheduler))
		scheduler <- UrlRequestScheduler$new(n = 3, parent = .self)
	is(scheduler, "UrlRequestScheduler") || .self$message(MSG.ERROR, "The scheduler instance must inherit from UrlRequestScheduler class.")
	.scheduler <<- scheduler
})

# Get token {{{1
################################################################

RemotedbConn$methods( getToken = function() {
	return(.self$.token)
})

# Get URL scheduler {{{1
################################################################

RemotedbConn$methods( .getUrlScheduler = function() {
	"!!! PRIVATE METHOD !!! Returns the URL scheduler.
	returns: The URL scheduler."

	return(.self$.scheduler)
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

# Get entry content url {{{1
################################################################

RemotedbConn$methods( getEntryContentUrl = function(ids, max.length = 0) {
	"Get the contents of specified entry identifiers. 
	id: A character vector containing the identifiers.
	max.length: Maximum length of URL strings.
	return: A character vector containing the URL strings. NULL if no identifier is given (empty vector)."

	# Copy code from get.entry.url
	# 

	urls <- character(0)

	if (length(ids) > 0) {

		# Get full URL
		.self$message(MSG.DEBUG, "Getting full URL.")
		full.url <- .self$.doGetEntryContentUrl(ids)

		# No single URL for multiple IDs
		if (length(ids) > 1 && length(full.url) > 1) {
			.self$message(MSG.DEBUG, "Obtained more than one URL.")
			if (length(full.url) != length(ids))
				.self$message(MSG.ERROR, paste(".doGetEntryContentUrl() does not concatenate IDs to form a single URL. However it returns only ", length(full.url), " URLs for ", length(ids), " IDs. It should return the same number of URLs than IDs.", sep = ''))
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
			while (start <= length(ids)) {
				# Find max size URL
				a <- start
				b <- length(ids)
				while (a < b) {
					m <- as.integer((a + b) / 2)
					url <- .self$.doGetEntryContentUrl(ids[1:m])
					if (nchar(url) <= max.length && m != a)
						a <- m
					else
						b <- m
				}
				urls <- c(urls, .self$.doGetEntryContentUrl(ids[1:a]))
				start <- a + 1
			}
		}
	}

	return(urls)
})

# Do get entry content url {{{1
################################################################

RemotedbConn$methods( .doGetEntryContentUrl = function(id) {
	"Get the contents of specified entry identifiers. 
	id: A character vector containing the identifiers.
	return: A character vector containing the entry contents. NULL if no identifier is given (empty vector)."

	.self$.abstract.method()
})

# Get entry page url {{{1
################################################################

RemotedbConn$methods( getEntryPageUrl = function(id) {
	.self$.abstract.method()
})
