# vi: fdm=marker

# Class declaration {{{1
################################################################

#' Class for handling URL requests.
#'
#' This class handles GET and POST requests, as well as file downloading. Each remote database connection instance (instance of concrete class inheriting from \code{RemotedbConn}) creates an instance of \code{BiodbRequestScheduler} for handling database connection. A timer is used to schedule connections, and avoid sending too much requests to the database. This class is not meant to be used directly by the library user. See section Fields for a list of the constructor's parameters.
#'
#' @param url           The URL to access, as a character string.
#' @param soap.request  The XML SOAP request to send, as a character string. 
#' @param soap.action   The SOAP action to contact, as a character string.
#' @param params        The list of parameters to use with the URL.
#' @param method        The method to use: either 'get' or 'post'.
#' @param opts          The CURL options to use.
#' @param dest.file     A path to a destination file.
#'
#' @seealso \code{\link{RemotedbConn}}, \code{\link{BiodbRequestSchedulerRule}}.
#'
#' @import methods
#' @include ChildObject.R
#' @export BiodbRequestScheduler
#' @exportClass BiodbRequestScheduler
BiodbRequestScheduler <- methods::setRefClass("BiodbRequestScheduler", contains = "ChildObject", fields = list(.ssl.verifypeer = "logical", .nb.max.tries = "integer", .conn.ids = "character", .rules = "list"))

# Constructor {{{1
################################################################

BiodbRequestScheduler$methods( initialize = function(n = 1, t = 1, ...) {

	callSuper(...)

	.conn.ids <<- character()
	.rules <<- list()
	.nb.max.tries <<- 10L
	.ssl.verifypeer <<- TRUE
})

# Send soap request {{{1
################################################################

BiodbRequestScheduler$methods( sendSoapRequest = function(url, soap.request, soap.action = NA_character_, encoding = integer()) {
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

BiodbRequestScheduler$methods( getUrlString = function(url, params = list()) {
	":\n\nBuild a URL string, using a base URL and parameters to be passed."

	pn <- names(params)
	params.lst <- vapply(seq(params), function(n) if (is.null(pn) || nchar(pn[[n]]) == 0) params[[n]] else paste(pn[[n]], params[[n]], sep = '='), FUN.VALUE = '')
	params.str <- paste(params.lst, collapse = '&')
	url <- paste(url, params.str, sep = '?')
	return(url)
})

# Get URL {{{1
################################################################

BiodbRequestScheduler$methods( getUrl = function(url, params = list(), method = 'get', opts = .self$.get.curl.opts(), encoding = integer()) {
	":\n\nSend a URL request, either with GET or POST method, and return result."

	content <- NA_character_

	# Get rule
	rule <- .self$.findRule(url)
	if (is.null(rule))
		.self$message('caution', paste0('Cannot find any rule for URL "', url,'".'))

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
		content <- .self$getBiodb()$getCache()$loadFileContent(conn.id = 'request', subfolder = 'shortterm', name = request.key, ext ='content', output.vector = TRUE)
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
					rule$wait.as.needed()

					content <- RCurl::getURL(url, .opts = opts, ssl.verifypeer = .self$.ssl.verifypeer, .encoding = encoding)
					if (.self$getBiodb()$getConfig()$get('cache.all.requests'))
						.self$getBiodb()$getCache()$saveContentToFile(content, conn.id = method, subfolder = 'shortterm', name = request.key, ext = 'content')
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
					rule$wait.as.needed()

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
		if ( ! is.na(content) && .self$getBiodb()$getConfig()$isEnabled('cache.system') && .self$getBiodb()$getConfig()$get('cache.all.requests')) {
			.self$message('debug', paste0("Saving content of ", method, " request to cache ..."))
			.self$getBiodb()$getCache()$saveContentToFile(content, conn.id = 'request', subfolder = 'shortterm', name = request.key, ext ='content')
			.self$getBiodb()$getCache()$saveContentToFile(request.json.str, conn.id = 'request', subfolder = 'shortterm', name = request.key, ext ='desc')
		}
	}

	return(content)
})

# Download file {{{1
################################################################

BiodbRequestScheduler$methods( downloadFile = function(url, dest.file) {
	":\n\nDownload the content of a URL and save it into the specified destination file."

	# Get rule
	rule <- .self$.findRule(url)
	if (is.null(rule))
		.self$message('caution', paste0('Cannot find any rule for URL "', url,'".'))

	# Wait required time between two requests
	rule$wait.as.needed()

	utils::download.file(url = url, destfile = dest.file, mode = 'wb', method = 'libcurl', cacheOK = FALSE, quiet = TRUE)
})

# Private methods {{{1
################################################################

# Get curl options {{{2
################################################################

BiodbRequestScheduler$methods( .get.curl.opts = function(opts = list()) {
	opts <- RCurl::curlOptions(useragent = .self$getBiodb()$getConfig()$get('useragent'), timeout.ms = 60000, verbose = FALSE, .opts = opts)
	return(opts)
})

# Check offline mode {{{2
################################################################

BiodbRequestScheduler$methods( .check.offline.mode = function() {

	if (.self$getBiodb()$getConfig()$isEnabled('offline'))
		.self$message('error', "Offline mode is enabled. All connections are forbidden.")
})

## Wait for huge download as needed {{{2
#################################################################
#
#BiodbRequestScheduler$methods( .wait.for.huge.dwnld.as.needed = function() {
#
#	# Wait, if needed, before previous URL request and this new URL request.
#	if (.self$.time.of.last.huge.dwnld.request > 0) {
#		spent_time <- Sys.time() - .self$.time.of.last.huge.dwnld.request
#		if (spent_time < .self$.huge.download.waiting.time)
#			Sys.sleep(.self$.huge.download.waiting.time - spent_time)
#	}
#
#	# Store current time
#	.time.of.last.huge.dwnld.request <<- Sys.time()
#})

# Register connector {{{2
################################################################

BiodbRequestScheduler$methods( .registerConnector = function(conn) {

	if ( ! conn$getId() %in% .self$.conn.ids) {
		.conn.ids <<- c(.self$.conn.ids, conn$getId())

		# Register as observer
		conn$registerObserver(.self)

		# Update rules
		.self$.updateRules()
	}
})

# Unregister connector {{{2
################################################################

BiodbRequestScheduler$methods( .unregisterConnector = function(conn) {

	if (conn$getId() %in% .self$.conn.ids) {
		.conn.ids <<- .self$.conn.ids[.self$.conn.ids == conn$getId()]

		# Unregister as observer
		conn$unregisterObserver(.self)

		# Update rules
		.self$.updateRules()
	}
})

# Find rule {{{2
################################################################

BiodbRequestScheduler$methods( .findRule = function(url) {

	rule <- NULL

	# Look for rules with similar URL
	similar.urls <- vapply(.self$.rules, function(r) r$urlIsSimilar(url), FUN.VALUE = T)
	if (sum(similar.urls) > 1)
		.self$message('error', paste0('Found more than one rule for url "', url, '": ', paste(names(.self$.rules)[similar.urls], collapse = ', ')))
	if (any(similar.urls))
		rule <- .self$.rules[[which(similar.urls)]]

	return(rule)
})

# Update rules {{{2
################################################################

BiodbRequestScheduler$methods( .updateRules = function(conn) {

	# Loop on all connectors
	for (conn.id in .self$.conn.ids) {

		# Get connector
		conn <- .self$getBiodb()$getFactory()$getConn(conn.id)

		# Get timings
		n <- conn$getSchedulerNParam()
		t <- conn$getSchedulerTParam()

		# Loop on all urls
		for (url in conn$getUrls()) {

			# Check a rule already exists
			rule <- .self$.findRule(url)
			if (is.null(rule))
				rule <- BiodbRequestSchedulerRule$new(parent = .self, url = url, n = n, t = t)
			else {

				# Remove found rule
				.self.rules[[rule$url]] <- NULL

				# Update rule
				if (nchar(rule$getUrl()) > nchar(url))
					rule$setUrl(url)
				if ((abs(rule$getT() / rule$getN() - t / n) < 0.1 && rule$getN() > n) # equivalent rule
				    || rule$getT() / rule$getN() > t / n)
					rule$setFrequence(n = n, t = t)
			}

			# Set rule
			.self$.rules[[rule$url]] <- rule
		}
	}
})
