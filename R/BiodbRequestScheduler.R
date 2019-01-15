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
#' @seealso \code{\link{RemotedbConn}}, \code{\link{BiodbRequestSchedulerRule}}, \code{\link{BiodbConnObserver}}.
#'
#' @import methods
#' @include ChildObject.R
#' @include BiodbConnObserver.R
#' @export BiodbRequestScheduler
#' @exportClass BiodbRequestScheduler
BiodbRequestScheduler <- methods::setRefClass("BiodbRequestScheduler", contains = c("ChildObject", "BiodbConnObserver"), fields = list(.ssl.verifypeer = "logical", .nb.max.tries = "integer", .host2rule = "list", .connid2rules = "list"))

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

BiodbRequestScheduler$methods( getUrl = function(url, params = list(), method = 'get', opts = .self$.get.curl.opts(), encoding = NA_character_) {
	":\n\nSend a URL request, either with GET or POST method, and return result."

	content <- NA_character_

	# Get rule
	rule <- .self$.findRule(url)
#	if (is.null(rule))
#		.self$message('caution', paste0('Cannot find any rule for URL "', url,'".'))

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

			content <- tryCatch({

				.self$message('debug', paste0("Sending ", method, " request ..."))

				if (method == 'post' && 'httpheader' %in% names(opts))
					.self$message('debug', paste0(method, ' request header is "', paste(opts$httpheader, collapse = ', '), '".'))
				if (method == 'post' && 'postfields' %in% names(opts))
					.self$message('debug', paste0('"Request post content is "', paste(opts$postfields, collapse = ', '), '".'))

				# Check if in offline mode
				.self$.check.offline.mode()

				# Wait required time between two requests
				rule$wait.as.needed()

				if (method == 'get')
					content <- RCurl::getURL(url, .opts = opts, ssl.verifypeer = .self$.ssl.verifypeer, .encoding = if (is.na(encoding)) integer() else encoding)
				else
					content <- RCurl::postForm(url, .opts = opts, .params = params, .encoding = encoding)

				# Check content
				if (length(grep('The proxy server could not handle the request', content)) > 0) {
					.self$message('debug', 'Found proxy error message in content.')
					stop("Error between the proxy and the main server.") # This happens sometime with NCBI CCDS server.
				}

				content
			},
			error = function(e) {
				.self$message('info', paste("Connection error \"", e$message, "\"", sep = ''))
				.self$message('info', "Retrying connection to server...")
				return(NA_character_)
			} )

			# Leave the loop
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
#	if (is.null(rule))
#		.self$message('caution', paste0('Cannot find any rule for URL "', url,'".'))

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

# Extract domain from URL {{{2
################################################################

BiodbRequestScheduler$methods( .extractDomainfromUrl = function(url) {

	domain <- sub('^.+://([^/]+)(/.*)?$', '\\1', url, perl = TRUE)

	return(domain)
})

# Find rule {{{2
################################################################

BiodbRequestScheduler$methods( .findRule = function(url, fail = TRUE) {
	.self$.assert.not.null(url)
	.self$.assert.length.one(url)
	.self$.assert.not.na(url)
	domain <- .self$.extractDomainfromUrl(url)

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
			host <- .self$.extractDomainfromUrl(url)
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
