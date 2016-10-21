if ( ! exists('UrlRequestScheduler')) { # Do not load again if already loaded

	#############
	# CONSTANTS #
	#############

	RLIB.GET  <- 'GET'
	RLIB.POST <- 'POST'

	#####################
	# CLASS DECLARATION #
	#####################
	
	UrlRequestScheduler <- setRefClass("UrlRequestScheduler", fields = list(.n = "numeric", .t = "numeric", .time.of.last.request = "ANY", .useragent = "character", .ssl.verifypeer = "logical", .nb.max.tries = "integer", .verbose = "integer"))
	
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
	
	####################
	# GET CURL OPTIONS #
	####################
	
	UrlRequestScheduler$methods( .get_curl_opts = function(url) {
		opts <- curlOptions(useragent = .self$.useragent, timeout.ms = 60000, verbose = FALSE)
		return(opts)
	})
	
	###########
	# GET URL #
	###########
	
	UrlRequestScheduler$methods( .doGetUrl = function(url, params = NULL, method = RLIB.GET) {
	
		content <- NA_character_
	
		# Use form to send URL request
		if ( ! is.null(params) && ! is.na(params))
			switch(method,
			       GET = { content <- getForm(url, .opts = .self$.get_curl_opts(), .params = params) },
			       POST = { content <- postForm(url, .opts = .self$.get_curl_opts(), .params = params) },
			       stop(paste('Unknown method "', method, '".'))
			      )
	
		# Get URL normally
		else
			content <- getURL(url, .opts = .self$.get_curl_opts(), ssl.verifypeer = .self$.ssl.verifypeer)
	
		return(content)
	})
	
	UrlRequestScheduler$methods( getUrl = function(url, params = NULL, method = RLIB.GET) {
	
		# Load library here and not inside .doGetUrl() since it is called from inside a try/catch clause, hence if library is missing the error will be ignored.
		library(bitops)
		library(RCurl)
	
		content <- NA_character_
	
		# Wait required time between two requests
		.self$.wait.as.needed()
	
		# Run query
		for (i in seq(.self$.nb.max.tries)) {
			tryCatch({ content <- .self$.doGetUrl(url, params = params, method = method) },
			         error = function(e) { if (.self$.verbose > 0) print("Retry connection to server...") } )
			if ( ! is.na(content))
				break
		}
	
		return(content)
	})
}
