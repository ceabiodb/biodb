# vi: fdm=marker

# Constants {{{1
################################################################

.HTTP.STATUS.OK <- 200
.HTTP.STATUS.NOT.FOUND <- 404
.HTTP.STATUS.REQUEST.TIMEOUT <- 408
.HTTP.STATUS.INTERNAL.SERVER.ERROR <- 500
.HTTP.STATUS.SERVICE.UNAVAILABLE <- 503

# Class declaration {{{1
################################################################

#' Class for handling requests.
#'
#' This class handles GET and POST requests, as well as file downloading. Each remote database connection instance (instance of concrete class inheriting from \code{BiodbRemotedbConn}) creates an instance of \code{BiodbRequestScheduler} for handling database connection. A timer is used to schedule connections, and avoid sending too much requests to the database. This class is not meant to be used directly by the library user. See section Fields for a list of the constructor's parameters.
#'
#' @param url           The URL to access, as a character string.
#' @param soap.request  The XML SOAP request to send, as a character string. 
#' @param soap.action   The SOAP action to contact, as a character string.
#' @param params        The list of parameters to use with the URL.
#' @param method        The method to use: either 'get' or 'post'.
#' @param opts          The CURL options to use.
#' @param dest.file     A path to a destination file.
#'
#' @seealso \code{\link{BiodbRemotedbConn}}, \code{\link{BiodbRequestSchedulerRule}}, \code{\link{BiodbConnObserver}}.
#'
#' @import methods
#' @include BiodbChildObject.R
#' @include BiodbConnObserver.R
#' @export BiodbRequestScheduler
#' @exportClass BiodbRequestScheduler
BiodbRequestScheduler <- methods::setRefClass("BiodbRequestScheduler", contains = c("BiodbChildObject", "BiodbConnObserver"), fields = list(.ssl.verifypeer = "logical", .nb.max.tries = "integer", .host2rule = "list", .connid2rules = "list"))

# Constructor {{{1
################################################################

BiodbRequestScheduler$methods( initialize = function(...) {

	callSuper(...)

	.connid2rules <<- list()
	.host2rule <<- list()
	.nb.max.tries <<- 10L
	.ssl.verifypeer <<- TRUE
})

# Send soap request {{{1
################################################################

BiodbRequestScheduler$methods( sendSoapRequest = function(url, soap.request, soap.action = NA_character_, encoding = integer()) {
	":\n\nSend a SOAP request to a URL. Returns the string result."

	# Prepare request
	header <- c(Accept = "text/xml", Accept = "multipart/*",  'Content-Type' = "text/xml; charset=utf-8")
	if ( ! is.na(soap.action))
		header <- c(header, SOAPAction = soap.action)

	# Send request
	results <- .self$getUrl(url, method = 'post', header = header, body = soap.request, encoding = encoding)

	return(results)
})

# Send request {{{1
################################################################

BiodbRequestScheduler$methods( sendRequest = function(request, cache.read = TRUE) {
	":\n\nSend a request, and return content result."

	content <- NA_character_

	# Get rule
	rule <- .self$.findRule(request$getUrl())

	# Log URL
	.self$message('debug', paste0("Getting content of ", request$getMethod(), " URL request \"", request$getUrl()$toString(encode = FALSE), "\"."))

	# Try to get query result from cache
	request.key <- request$getUniqueKey()
	if (cache.read && .self$getBiodb()$getConfig()$isEnabled('cache.system') && .self$getBiodb()$getConfig()$get('cache.all.requests') && .self$getBiodb()$getCache()$fileExist('request', subfolder = 'shortterm', name = request.key, ext = 'content')) {
		.self$message('debug', "Loading content of request from cache.")
		content <- .self$getBiodb()$getCache()$loadFileContent('request', subfolder = 'shortterm', name = request.key, ext ='content', output.vector = TRUE)
	}

	# Check if in offline mode
	.self$.check.offline.mode()

	if (is.na(content)) {

		content <- .self$.doSendRequest(request = request, rule = rule)

		# Save content to cache
		if ( ! is.na(content) && .self$getBiodb()$getConfig()$isEnabled('cache.system') && .self$getBiodb()$getConfig()$get('cache.all.requests')) {
			.self$message('debug', "Saving content of request to cache.")
			.self$getBiodb()$getCache()$saveContentToFile(content, cache.id = 'request', subfolder = 'shortterm', name = request.key, ext ='content')
			.self$getBiodb()$getCache()$saveContentToFile(request$toString(), cache.id = 'request', subfolder = 'shortterm', name = request.key, ext ='desc')
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

	# Wait required time between two requests
	rule$wait.as.needed()

	utils::download.file(url = url, destfile = dest.file, mode = 'wb', method = 'libcurl', cacheOK = FALSE, quiet = TRUE)
})

# Private methods {{{1
################################################################

# Connector observer methods {{{2
################################################################

# Terminating {{{3
################################################################

BiodbRequestScheduler$methods( connTerminating = function(conn) {
	.self$.unregisterConnector(conn)
})

# URLs updated {{{3
################################################################

BiodbRequestScheduler$methods( connUrlsUpdated = function(conn) {
	.self$.unregisterConnector(conn)
	.self$.registerConnector(conn)
})

# Scheduler frequency updated {{{3
################################################################

BiodbRequestScheduler$methods( connSchedulerFrequencyUpdated = function(conn) {

	# Is connector not registered?
	if ( ! conn$getId() %in% names(.self$.connid2rules))
		.self$message('caution', paste0('Connector "', conn$getId(), '" has never been registered.'))

	# Update frequency
	else {
		for (rule in .self$.connid2rules[[conn$getId()]])
			rule$recomputeFrequency()
	}
})


# Check offline mode {{{2
################################################################

BiodbRequestScheduler$methods( .check.offline.mode = function() {

	if (.self$getBiodb()$getConfig()$isEnabled('offline'))
		.self$message('error', "Offline mode is enabled. All connections are forbidden.")
})


# Register connector {{{2
################################################################

BiodbRequestScheduler$methods( .registerConnector = function(conn) {

	# Is connector already registered?
	if (conn$getId() %in% names(.self$.connid2rules))
		.self$message('caution', paste0('Connector "', conn$getId(), '" has already been registered.'))

	# Add connector
	else {
		# Register as observer
		conn$.registerObserver(.self)

		# Add connector
		.self$.addConnectorRules(conn)
	}
})

# Unregister connector {{{2
################################################################

BiodbRequestScheduler$methods( .unregisterConnector = function(conn) {

	# Is connector not registered?
	if ( ! conn$getId() %in% names(.self$.connid2rules))
		.self$message('caution', paste0('Connector "', conn$getId(), '" has never been registered.'))

	# Unregister connector
	else {
		# Unregister as observer
		conn$.unregisterObserver(.self)

		# Remove connector
		.self$.removeConnectorRules(conn)
	}
})


# Find rule {{{2
################################################################

BiodbRequestScheduler$methods( .findRule = function(url, fail = TRUE) {

	.self$.assert.not.null(url)
	if ( ! is(url, 'BiodbUrl')) {
		.self$.assert.length.one(url)
		.self$.assert.not.na(url)
		url <- BiodbUrl(url =url)
	}
	domain <- url$getDomain()

	# Rule does not exist
	if (fail && ! domain %in% names(.self$.host2rule))
		.self$message('error', paste0('No rule exists for domain "', domain, '".'))

	return(.self$.host2rule[[domain]])
})

# Add connector rules {{{2
################################################################

BiodbRequestScheduler$methods( .addConnectorRules = function(conn) {

	.self$.connid2rules[[conn$getId()]] <- list()

	# Loop on all connector URLs
	for (url in conn$getUrls()) {

		# Check if a rule already exists
		rule <- .self$.findRule(url, fail = FALSE)

		# No rule exists => create new one
		if (is.null(rule)) {
			host <- BiodbUrl(url =url)$getDomain()
			.self$message('debug', paste0('Create new rule for URL "', host,'" of connector "', conn$getId(), '"'))
			rule <- BiodbRequestSchedulerRule$new(parent = .self, host = host, conn = conn)
			.self$.host2rule[[rule$getHost()]] <- rule
		}

		# A rule with the same host already exists, add connector to it
		else
			rule$addConnector(conn)

		# Add rule
		if ( ! any(vapply(.self$.connid2rules[[conn$getId()]], function(x) identical(rule, x), FUN.VALUE = TRUE)))
			.self$.connid2rules[[conn$getId()]] <- c(.self$.connid2rules[[conn$getId()]], rule)
	}
})

# Get all rules {{{2
################################################################

BiodbRequestScheduler$methods( .getAllRules = function() {
	return(.self$.host2rule)
})

# Get connector rules {{{2
################################################################

BiodbRequestScheduler$methods( .getConnectorRules = function(conn) {
	.self$.assert.not.null(conn)
	return(.self$.connid2rules[[conn$getId()]])
})

# Remove connector rules {{{2
################################################################

BiodbRequestScheduler$methods( .removeConnectorRules = function(conn) {

	# Get rules
	rules <- .self$.connid2rules[[conn$getId()]]

	# Loop on connector rules
	for (rule in rules) {

		if (length(rule$getConnectors()) == 1)
			.self$.host2rule[[rule$getHost()]] <- NULL
		else
			rule$removeConnector(conn)
	}
	
	# Remove connector
	.self$.connid2rules[[conn$getId()]] <- NULL
})

# Do send request {{{2
################################################################

BiodbRequestScheduler$methods( .doSendRequest = function(request, rule) {

	content <- NA_character_

	# Create HTTP header object (to receive HTTP information from server).
	header <- RCurl::basicHeaderGatherer()

	# Enter query loop
	i = 0
	retry = TRUE
	while (retry && i < .self$.nb.max.tries) {

		err_msg = NULL

		# Increment try number
		i = i + 1

		# Print debug information about header and body
		.self$message('debug', paste0('Request header is: "', request$getHeaderAsSingleString(), '".'))
		.self$message('debug', paste0('Request body is "', paste(request$getBody(), collapse = ', '), '".'))

		# Wait required time between two requests
		rule$wait.as.needed()

		# Build options
		opts <- request$getCurlOptions(useragent = .self$getBiodb()$getConfig()$get('useragent'))

		# Send request
		retry = FALSE
		curl.error = NULL
		header$reset()
		content = tryCatch(expr = {
				if (request$getMethod() == 'get')
					RCurl::getURL(request$getUrl()$toString(), .opts = opts, ssl.verifypeer = .self$.ssl.verifypeer, .encoding = request$getEncoding(), headerfunction = header$update)
				else
					RCurl::postForm(request$getUrl()$toString(), .opts = opts, .encoding = request$getEncoding(), headerfunction = header$update)
				},
			PEER_FAILED_VERIFICATION = function(err) { retry = TRUE ; curl.error = err },
			GenericCurlError = function(err) { retry = TRUE ; curl.error = err },
			error = function(err) { retry = FALSE ; curl.error = err })

		# RCurl error
		if ( ! is.null(curl.error))
			err_msg = paste0("RCurl error: ", curl.error)

		# Get header information sent by server
		hdr = NULL
		if (is.null(err_msg)) {
			hdr = tryCatch(expr = as.list(header$value()),
			               warning = function(w) w, # We want to catch "<simpleWarning in max(i): no non-missing arguments to max; returning -Inf>".
			               error = function(e) e)

			if (methods::is(hdr, 'simpleError') || methods::is(hdr, 'simpleWarning')) {
				err_msg = paste0('Error while retrieving HTTP header: ', hdr, '.')
				hdr = NULL
				retry = TRUE
			}

			if ( ! is.null(hdr)) {
				hdr$status <- as.integer(hdr$status)
				if (hdr$status == 0) {
					hdr = NULL
					err_msg = "Cannot find status info in HTTP header."
					retry = TRUE
				}
			}
		}

		# Recoverable HTTP errors
		if ( ! is.null(hdr) && hdr$status %in% c(.HTTP.STATUS.NOT.FOUND, .HTTP.STATUS.REQUEST.TIMEOUT, .HTTP.STATUS.INTERNAL.SERVER.ERROR, .HTTP.STATUS.SERVICE.UNAVAILABLE)) {
			err_msg = paste0("HTTP error ", hdr$status," (\"", hdr$statusMessage, "\").")
			if ('Retry-After' %in% names(hdr))
				err_msg = paste0(err_msg, " Retry after ", hdr[['Retry-After']], ".")
			retry = TRUE
		}

		# Other HTTP errors
		if (is.null(err_msg) && ! is.null(hdr) && hdr$status != .HTTP.STATUS.OK) {
			err_msg = paste0("Unrecoverable HTTP error ", hdr$status," (\"", hdr$statusMessage, "\").")
			if ('Retry-After' %in% names(hdr))
				err_msg = paste0(err_msg, " Retry after ", hdr[['Retry-After']], ".")
			content = NA_character_
			retry = FALSE
		}

		# Proxy server error
		if (is.null(err_msg) && ! is.null(content) && ! is.na(content) && length(grep('The proxy server could not handle the request', content)) > 0) {
			.self$message('debug', 'Found proxy error message in content.')
			err_msg = "Error between the proxy and the main server." # This happens sometime with NCBI CCDS server.
			content = NA_character_
			retry = FALSE
		}

		# Message
		if ( ! is.null(err_msg)) {
			content = NA_character_
			if (retry)
				err_msg = paste0(err_msg, " Retrying connection to server...")
			.self$message('info', err_msg)
		}

		if (retry)
			content = NA_character_
	}

	return(content)
})

# Deprecated methods {{{1
################################################################

# Get URL string {{{2
################################################################

BiodbRequestScheduler$methods( getUrlString = function(url, params = list()) {
	":\n\nBuild a URL string, using a base URL and parameters to be passed."

	.self$.deprecated.method("BiodbUrl::toString()")

	url <- BiodbUrl(url = url, params = params)$toString(encode = FALSE)

	return(url)
})

# Get URL {{{2
################################################################

BiodbRequestScheduler$methods( getUrl = function(url, params = list(), method = c('get', 'post'), header = character(), body = character(), encoding = integer()) {
	":\n\nSend a URL request, either with GET or POST method, and return result."

	.self$.deprecated.method("BiodbRequestScheduler::sendRequest()")

	method <- match.arg(method)

	request <- BiodbRequest(url = BiodbUrl(url = url, params = params), method = method, header = header, body = body, encoding = encoding)

	return(.self$sendRequest(request))
})

