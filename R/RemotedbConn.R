# vi: fdm=marker

# Class declaration {{{1
################################################################

RemotedbConn <- methods::setRefClass("RemotedbConn", contains = "BiodbConn", fields = list(.scheduler = "UrlRequestScheduler", .token = "character", .base.url = "character"))

# Constructor {{{1
################################################################

RemotedbConn$methods( initialize = function(scheduler = NULL, token = NA_character_, base.url = NA_character_, ...) {

	# Set base URL
	.base.url <<- base.url

	# Set token
	.token <<- token

	# Set scheduler
	if (is.null(scheduler))
		scheduler <- UrlRequestScheduler$new(n = 3, parent = .self)
	is(scheduler, "UrlRequestScheduler") || .self$message(MSG.ERROR, "The scheduler instance must inherit from UrlRequestScheduler class.")
	.scheduler <<- scheduler

	callSuper(...)
})

# Get base url {{{1
################################################################

RemotedbConn$methods( getBaseUrl = function() {
	return(.self$.base.url)
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
	return(.self$.scheduler$getUrl(url))
})

# Set user agent {{{1
################################################################

RemotedbConn$methods( .set.useragent = function(useragent) {
	.scheduler$setUserAgent(useragent) # set agent
})

# Get entry content url {{{1
################################################################

RemotedbConn$methods( getEntryContentUrl = function(id) {
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
