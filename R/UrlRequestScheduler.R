# vi: fdm=marker

# CONSTANTS {{{1
################################################################

BIODB.GET  <- 'GET'
BIODB.POST <- 'POST'

# CLASS DECLARATION {{{1
################################################################

UrlRequestScheduler <- methods::setRefClass("UrlRequestScheduler", contains = "BiodbObject", fields = list(.n = "numeric", .t = "numeric", .time.of.last.request = "ANY", .ssl.verifypeer = "logical", .nb.max.tries = "integer", .verbose = "integer", .parent = "ANY"))

# n: number of connections
# t: time (in seconds)

# The scheduler restrict the number of connections at n per t seconds.

# CONSTRUCTOR {{{1
################################################################

UrlRequestScheduler$methods( initialize = function(n = 1, t = 1, ssl.verifypeer = TRUE, parent = NULL, ...) {
	is.null(parent) || is(parent, "BiodbObject") || .self$message(MSG.ERROR, paste0("Parent must be of class BiodbObject, it was of class ", class(parent), "."))
	.parent <<- parent
	.n <<- n
	.t <<- t
	.time.of.last.request <<- -1
	.nb.max.tries <<- 10L
	.ssl.verifypeer <<- ssl.verifypeer
	.verbose <<- 0L
	callSuper(...) # calls super-class initializer with remaining parameters
})

# GET BIODB {{{1
################################################################

UrlRequestScheduler$methods( getBiodb = function() {
	biodb <- if (is.null(.self$.parent)) NULL else .self$.parent$getBiodb()
	return(biodb)
})

# SET VERBOSE {{{1
################################################################

UrlRequestScheduler$methods( setVerbose = function(verbose) {
	.verbose <<- verbose
})

# WAIT AS NEEDED {{{1
################################################################

# Wait the specified between two requests.
UrlRequestScheduler$methods( .wait.as.needed = function() {

	# Compute minimum waiting time between two URL requests
	waiting_time <- .self$.t / .self$.n

	# Wait, if needed, before previous URL request and this new URL request.
	if (.self$.time.of.last.request > 0) {
		spent_time <- Sys.time() - .self$.time.of.last.request
		if (spent_time < waiting_time)
			Sys.sleep(waiting_time - spent_time)
	}

	# Store current time
	.time.of.last.request <<- Sys.time()
})

# GET CURL OPTIONS {{{1
################################################################

UrlRequestScheduler$methods( .get.curl.opts = function(opts = list()) {
	opts <- RCurl::curlOptions(useragent = .self$getUserAgent(), timeout.ms = 60000, verbose = FALSE, .opts = opts)
	return(opts)
})

# DO GET URL {{{1
################################################################

UrlRequestScheduler$methods( .doGetUrl = function(url, params = list(), method = BIODB.GET, opts = .self$.get.curl.opts()) {

	content <- NA_character_

	# Use form to send URL request
	if ( method == BIODB.POST || ( ! is.null(params) && ! is.na(params) && length(params) > 0)) {
		switch(method,
			   GET = { content <- RCurl::getForm(url, .opts = opts, .params = params) },
			   POST = { content <- RCurl::postForm(url, .opts = opts, .params = params) },
			   stop(paste('Unknown method "', method, '".'))
			  )
	}

	# Get URL normally
	else {
		content <- RCurl::getURL(url, .opts = opts, ssl.verifypeer = .self$.ssl.verifypeer)
	}
	return(content)
})

# SEND SOAP REQUEST {{{1
################################################################

UrlRequestScheduler$methods( sendSoapRequest = function(url, request) {
	header <- c(Accept="text/xml", Accept="multipart/*",  'Content-Type'="text/xml; charset=utf-8")
	opts <- .self$.get.curl.opts(list(httpheader = header, postfields = request))
	results <- .self$getUrl(url, method = BIODB.POST, opts = opts)
	return(results)
})

# GET URL {{{1
################################################################

UrlRequestScheduler$methods( getUrl = function(url, params = list(), method = BIODB.GET, opts = .self$.get.curl.opts()) {

	.self$message(MSG.DEBUG, paste0("Sending URL request '", url, "'..."))

	content <- NA_character_

	# Wait required time between two requests
	.self$.wait.as.needed()

	# Run query
	for (i in seq(.self$.nb.max.tries)) {
		tryCatch({ content <- .self$.doGetUrl(url, params = params, method = method, opts = opts) },
			     error = function(e) { if (.self$.verbose > 0) print("Retry connection to server...") } )
		if ( ! is.na(content))
			break
	}

	return(content)
})
