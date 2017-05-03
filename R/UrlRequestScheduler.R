# vi: fdm=marker

#' @include ChildObject.R

# CONSTANTS {{{1
################################################################

BIODB.GET  <- 'GET'
BIODB.POST <- 'POST'

# CLASS DECLARATION {{{1
################################################################

UrlRequestScheduler <- methods::setRefClass("UrlRequestScheduler", contains = "ChildObject", fields = list(.n = "numeric", .t = "numeric", .time.of.last.request = "ANY", .ssl.verifypeer = "logical", .nb.max.tries = "integer", .huge.download.waiting.time = "integer", .time.of.last.huge.dwnld.request = "ANY"))

# n: number of connections
# t: time (in seconds)

# The scheduler restrict the number of connections at n per t seconds.

# Constructor {{{1
################################################################

UrlRequestScheduler$methods( initialize = function(n = 1, t = 1, ssl.verifypeer = TRUE, ...) {

	callSuper(...)

	.n <<- n
	.t <<- t
	.time.of.last.request <<- -1
	.nb.max.tries <<- 10L
	.ssl.verifypeer <<- ssl.verifypeer
	.huge.download.waiting.time <<- 60L # in seconds
	.time.of.last.huge.dwnld.request <<- -1
})

# Wait as needed {{{1
################################################################

# Wait enough time between two requests.
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
	opts <- RCurl::curlOptions(useragent = .self$getBiodb()$getConfig()$get(CFG.USERAGENT), timeout.ms = 60000, verbose = FALSE, .opts = opts)
	return(opts)
})

# Check offline mode {{{1
################################################################

UrlRequestScheduler$methods( .check.offline.mode = function() {

	if (.self$getBiodb()$getConfig()$isEnabled(CFG.OFFLINE))
		.self$message(MSG.ERROR, "Offline mode is enabled. All connections are forbidden.")
})

# SEND SOAP REQUEST {{{1
################################################################

UrlRequestScheduler$methods( sendSoapRequest = function(url, request, action = NA_character_) {

	.self$.check.offline.mode()

	header <- c(Accept = "text/xml", Accept = "multipart/*",  'Content-Type' = "text/xml; charset=utf-8")
	if ( ! is.na(action))
		header <- c(header, c(SOAPAction = action))
	opts <- .self$.get.curl.opts(list(httpheader = header, postfields = request))
	results <- .self$getUrl(url, method = BIODB.POST, opts = opts)
	return(results)
})

# Get url {{{1
################################################################

UrlRequestScheduler$methods( getUrl = function(url, params = list(), method = BIODB.GET, opts = .self$.get.curl.opts()) {

	content <- NA_character_

	# Check method
	if ( ! method %in% c(BIODB.GET, BIODB.POST))
		.self$message(MSG.ERROR, paste('Unknown method "', method, '".', sep = ''))

	# Append params for GET method
	if (method == BIODB.GET && length(params) > 0) {
		pn <- names(params)
		params.lst <- vapply(seq(params), function(n) if (is.null(pn) || nchar(pn[[n]]) == 0) params[[n]] else paste(pn[[n]], params[[n]], sep = '='), FUN.VALUE = '')
		params.str <- paste(params.lst, collapse = '&')
		url <- paste(url, params.str, sep = '?')
		params <- list()
	}

	# Log URL
	.self$message(MSG.DEBUG, paste0("Sending URL request \"", url, "\" with ", method, " method..."))

	# Check if in offline mode
	.self$.check.offline.mode()

	# Wait required time between two requests
	.self$.wait.as.needed()

	# Run query
	for (i in seq(.self$.nb.max.tries)) {
		tryCatch({
			if (method == BIODB.GET)
				content <- RCurl::getURL(url, .opts = opts, ssl.verifypeer = .self$.ssl.verifypeer)
			else
				content <- RCurl::postForm(url, .opts = opts, .params = params)
		},
			error = function(e) {
				.self$message(MSG.INFO, paste("Connection error \"", e$message, "\"", sep = ''))
				.self$message(MSG.INFO, "Retrying connection to server...")
			} )
		if ( ! is.na(content))
			break
	}

	return(content)
})

# Wait for huge download as needed {{{1
################################################################

UrlRequestScheduler$methods( .wait.for.huge.dwnld.as.needed = function() {

	# Wait, if needed, before previous URL request and this new URL request.
	if (.self$.time.of.last.huge.dwnld.request > 0) {
		spent_time <- Sys.time() - .self$.time.of.last.huge.dwnld.request
		if (spent_time < .huge.dwnld.waiting.time)
			Sys.sleep(.huge.dwnld.waiting.time - spent_time)
	}

	# Store current time
	.time.of.last.huge.dwnld.request <<- Sys.time()
})

# Download file {{{1
################################################################

UrlRequestScheduler$methods( downloadFile = function(url, dest.file) {

	# Wait required time between two requests
	.self$.wait.for.huge.dwnld.as.needed()

	utils::download.file(url = url, destfile = dest.file, mode = 'wb', method = 'libcurl', cacheOK = FALSE, quiet = TRUE)
})
