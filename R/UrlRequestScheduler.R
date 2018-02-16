# vi: fdm=marker

# Class declaration {{{1
################################################################

#' Class for handling URL requests.
#'
#' This class handles GET and POST requests, as well as file downloading. Each remote database connection instance (instance of concrete class inheriting from \code{RemotedbConn}) creates an instance of \code{UrlRequestScheduler} for handling database connection. A timer is used to schedule connections, and avoid sending too much requests to the database. This class is not meant to be used directly by the library user. See section Fields for a list of the constructor's parameters.
#'
#' @field n The number of connections allowed for each t seconds.
#' @field t The number of seconds during which n connections are allowed.
#' 
#' @param url           The URL to access, as a character string.
#' @param soap.request  The XML SOAP request to send, as a character string. 
#' @param soap.action   The SOAP action to contact, as a character string.
#' @param params        The list of parameters to use with the URL.
#' @param method        The method to use: either 'get' or 'post'.
#' @param opts          The CURL options to use.
#' @param dest.file     A path to a destination file.
#'
#' @seealso \code{\link{RemotedbConn}}.
#'
#' @import methods
#' @include ChildObject.R
#' @export UrlRequestScheduler
#' @exportClass UrlRequestScheduler
UrlRequestScheduler <- methods::setRefClass("UrlRequestScheduler", contains = "ChildObject", fields = list(.n = "numeric", .t = "numeric", .time.of.last.request = "ANY", .ssl.verifypeer = "logical", .nb.max.tries = "integer", .huge.download.waiting.time = "integer", .time.of.last.huge.dwnld.request = "ANY"))

# Constructor {{{1
################################################################

UrlRequestScheduler$methods( initialize = function(n = 1, t = 1, ...) {

	callSuper(...)

	.n <<- n
	.t <<- t
	.time.of.last.request <<- -1
	.nb.max.tries <<- 10L
	.ssl.verifypeer <<- TRUE
	.huge.download.waiting.time <<- 60L # in seconds
	.time.of.last.huge.dwnld.request <<- -1
})

# Send soap request {{{1
################################################################

UrlRequestScheduler$methods( sendSoapRequest = function(url, soap.request, soap.action = NA_character_, encoding = integer()) {
	":\n\nSend a SOAP request to a URL. Returns the string result."

	.self$.check.offline.mode()

	# Prepare request
	header <- c(Accept = "text/xml", Accept = "multipart/*",  'Content-Type' = "text/xml; charset=utf-8")
	if ( ! is.na(soap.action))
		header <- c(header, c(SOAPAction = soap.action))
	opts <- .self$.get.curl.opts(list(httpheader = header, postfields = soap.request))

	# Send request
	results <- .self$getUrl(url, method = 'post', opts = opts, encoding = encoding)

	return(results)
})

# Get URL string {{{1
################################################################

UrlRequestScheduler$methods( getUrlString = function(url, params = list()) {
	pn <- names(params)
	params.lst <- vapply(seq(params), function(n) if (is.null(pn) || nchar(pn[[n]]) == 0) params[[n]] else paste(pn[[n]], params[[n]], sep = '='), FUN.VALUE = '')
	params.str <- paste(params.lst, collapse = '&')
	url <- paste(url, params.str, sep = '?')
	return(url)
})

# Get url {{{1
################################################################

UrlRequestScheduler$methods( getUrl = function(url, params = list(), method = 'get', opts = .self$.get.curl.opts(), encoding = integer()) {
	":\n\nSend a URL request, either with GET or POST method, and return result."

	content <- NA_character_

	# Check method
	if ( ! method %in% c('get', 'post'))
		.self$message('error', paste('Unknown method "', method, '".', sep = ''))

	# Append params for GET method
	if (method == 'get' && length(params) > 0) {
		url <- .self$getUrlString(url, params)
		params <- list()
	}

	# Encode URL
	url <- URLencode(url)

	# Log URL
	.self$message('debug', paste0("Getting content of ", method, " URL request \"", url, "\" ..."))

	content <- NA_character_

	# Try to get query result from cache
	# Key built from method + url + params + opts
	request <- list(url = url, params = params, opts = opts)
	request.json <- jsonlite::serializeJSON(request)
	request.json.str <- as.character(request.json)
	request.key <- digest::digest(request.json.str, algo = 'md5')
	if (.self$getBiodb()$getConfig()$get('cache.all.requests') && .self$getBiodb()$getCache()$fileExist('request', subfolder = 'shortterm', name = request.key, ext = 'content')) {
		.self$message('debug', paste0("Loading content of ", method, " request from cache ..."))
		content <- .self$getBiodb()$getCache()$loadFileContent(dbid = 'request', subfolder = 'shortterm', name = request.key, ext ='content', output.vector = TRUE)
	}

	if (is.na(content)) {
		# Run query
		for (i in seq(.self$.nb.max.tries)) {
			tryCatch({

				# GET method
				if (method == 'get') {
					.self$message('debug', paste0("Sending ", method, " request ..."))

					# Check if in offline mode
					.self$.check.offline.mode()

					# Wait required time between two requests
					.self$.wait.as.needed()

					content <- RCurl::getURL(url, .opts = opts, ssl.verifypeer = .self$.ssl.verifypeer, .encoding = encoding)
					if (.self$getBiodb()$getConfig()$get('cache.all.requests'))
						.self$getBiodb()$getCache()$saveContentToFile(content, dbid = method, subfolder = 'shortterm', name = request.key, ext = 'content')
				}

				# POST method
				else {
					.self$message('debug', paste0("Sending ", method, " request..."))
					if ('httpheader' %in% names(opts))
						.self$message('debug', paste0(method, ' request header is "', paste(opts$httpheader, collapse = ', '), '".'))
					if ('postfields' %in% names(opts))
						.self$message('debug', paste0('"Request post content is "', paste(opts$postfields, collapse = ', '), '".'))

					# Check if in offline mode
					.self$.check.offline.mode()

					# Wait required time between two requests
					.self$.wait.as.needed()

					content <- RCurl::postForm(url, .opts = opts, .params = params, .encoding = encoding)
				}
			},
				error = function(e) {
					.self$message('info', paste("Connection error \"", e$message, "\"", sep = ''))
					.self$message('info', "Retrying connection to server...")
				} )
			if ( ! is.na(content))
				break
		}

		# Save content to cache
		if ( ! is.na(content) && .self$getBiodb()$getConfig()$get('cache.all.requests')) {
			.self$message('debug', paste0("Saving content of ", method, " request to cache ..."))
			.self$getBiodb()$getCache()$saveContentToFile(content, dbid = 'request', subfolder = 'shortterm', name = request.key, ext ='content')
			.self$getBiodb()$getCache()$saveContentToFile(request.json.str, dbid = 'request', subfolder = 'shortterm', name = request.key, ext ='desc')
		}
	}

	return(content)
})

# Download file {{{1
################################################################

UrlRequestScheduler$methods( downloadFile = function(url, dest.file) {
	":\n\nDownload the content of a URL and save it into the specified destination file."

	# Wait required time between two requests
	.self$.wait.for.huge.dwnld.as.needed()

	utils::download.file(url = url, destfile = dest.file, mode = 'wb', method = 'libcurl', cacheOK = FALSE, quiet = TRUE)
})

# Private methods {{{1
################################################################

# Wait as needed {{{2
################################################################

# Wait enough time between two requests.
UrlRequestScheduler$methods( .wait.as.needed = function() {

	# Compute minimum waiting time between two URL requests
	waiting_time <- .self$.t / .self$.n

	# Wait, if needed, before previous URL request and this new URL request.
	if (.self$.time.of.last.request > 0) {
		spent_time <- Sys.time() - .self$.time.of.last.request
		if (spent_time < waiting_time) {
			sleep.time <- waiting_time - spent_time
			.self$message('debug', paste('Wait ', sleep.time, '.', sep = ''))
			Sys.sleep(sleep.time)
		}
	}

	# Store current time
	.time.of.last.request <<- Sys.time()
})

# Get curl options {{{2
################################################################

UrlRequestScheduler$methods( .get.curl.opts = function(opts = list()) {
	opts <- RCurl::curlOptions(useragent = .self$getBiodb()$getConfig()$get('useragent'), timeout.ms = 60000, verbose = FALSE, .opts = opts)
	return(opts)
})

# Check offline mode {{{2
################################################################

UrlRequestScheduler$methods( .check.offline.mode = function() {

	if (.self$getBiodb()$getConfig()$isEnabled('offline'))
		.self$message('error', "Offline mode is enabled. All connections are forbidden.")
})

# Wait for huge download as needed {{{2
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
