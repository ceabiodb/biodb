library(bitops)
library(RCurl)

#####################
# CLASS DECLARATION #
#####################

UrlRequestScheduler <- setRefClass("UrlRequestScheduler", fields = list(n = "numeric", t = "numeric", time_of_last_request = "ANY"))

# n: number of connections
# t: time (in seconds)

# The scheduler restrict the number of connections at n per t seconds.

###############
# CONSTRUCTOR #
###############

UrlRequestScheduler$methods( initialize = function(n = 1, t = 1, ...) {
	n <<- n
	t <<- t
	time_of_last_request <<- -1
	callSuper(...) # calls super-class initializer with remaining parameters
})

###########
# GET URL #
###########

UrlRequestScheduler$methods( getUrl = function(url, useragent) {

	# Compute minimum waiting time between two URL requests
	waiting_time <- .self$t / .self$n

	# Wait, if needed, before previous URL request and this new URL request.
	if (.self$time_of_last_request > 0) {
		spent_time <- Sys.time() - .self$time_of_last_request
		if (spent_time < waiting_time)
			Sys.sleep(waiting_time - spent_time)
	}

	# Store current time
	time_of_last_request <<- Sys.time()

	# Send URL request and return results
	return(getURL(url, useragent = useragent))
})
