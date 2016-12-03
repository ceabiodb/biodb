#############
# CONSTANTS #
#############

BIODB.GET  <- 'GET'
BIODB.POST <- 'POST'

#####################
# CLASS DECLARATION #
#####################

UrlRequestScheduler <- methods::setRefClass("UrlRequestScheduler", fields = list(.n = "numeric", .t = "numeric", .time.of.last.request = "ANY", .useragent = "character", .ssl.verifypeer = "logical", .nb.max.tries = "integer", .verbose = "integer"))

# n: number of connections
# t: time (in seconds)

# The scheduler restrict the number of connections at n per t seconds.

###############
# CONSTRUCTOR #
###############

UrlRequestScheduler$methods( initialize = function(n = 1, t = 1, useragent = NA_character_, ssl.verifypeer = TRUE, ...) {
	.n <<- n
	.t <<- t
	.time.of.last.request <<- -1
	.useragent <<- useragent
	.nb.max.tries <<- 10L
	.ssl.verifypeer <<- ssl.verifypeer
	.verbose <<- 0L
	callSuper(...) # calls super-class initializer with remaining parameters
})

##################
# SET USER AGENT #
##################

UrlRequestScheduler$methods( setUserAgent = function(useragent) {
	.useragent <<- useragent
})

###############
# SET VERBOSE #
###############

UrlRequestScheduler$methods( setVerbose = function(verbose) {
	.verbose <<- verbose
})

##################
# WAIT AS NEEDED #
##################

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

#########################
# GET CURL OPTIONS {{{1 #
#########################

UrlRequestScheduler$methods( .get.curl.opts = function(opts = list()) {
	opts <- RCurl::curlOptions(useragent = .self$.useragent, timeout.ms = 60000, verbose = FALSE, .opts = opts)
	return(opts)
})

###################
# DO GET URL {{{1 #
###################

UrlRequestScheduler$methods( .doGetUrl = function(url, params = list(), method = BIODB.GET, opts = .self$.get.curl.opts()) {

	content <- NA_character_

	# Use form to send URL request
	print('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
	print(url)
	print(opts)
	print(method)
	print(params)
	if ( method == BIODB.POST || ( ! is.null(params) && ! is.na(params) && length(params) > 0)) {
		switch(method,
			   GET = { content <- getForm(url, .opts = opts, .params = params) },
			   POST = { content <- postForm(url, .opts = opts, .params = params) },
			   stop(paste('Unknown method "', method, '".'))
			  )
	print('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')

	# Get URL normally
	}else{
		content <- getURL(url, .opts = opts, ssl.verifypeer = .self$.ssl.verifypeer)
	}
	return(content)
})

##########################
# SEND SOAP REQUEST {{{1 #
##########################

UrlRequestScheduler$methods( sendSoapRequest = function(url, request) {
	print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
	print('sendSoapRequest')
	header <- c(Accept="text/xml", Accept="multipart/*",  'Content-Type'="text/xml; charset=utf-8")
	print(header)
	opts <- .self$.get.curl.opts(list(httpheader = header, postfields = request))
	print(opts)
	results <- .self$getUrl(url, method = BIODB.POST, opts = opts)
	print('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
	return(results)
})

################
# GET URL {{{1 #
################

UrlRequestScheduler$methods( getUrl = function(url, params = list(), method = BIODB.GET, opts = .self$.get.curl.opts()) {

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
