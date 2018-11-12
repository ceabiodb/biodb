# vi: fdm=marker

# Class declaration {{{1
################################################################

#' Scheduler rule class.
#'
#' This class represents a rule for the request scheduler. 
#'
#' @field n The number of connections allowed for each t seconds.
#' @field t The number of seconds during which n connections are allowed.
#' 
#' @seealso \code{\link{BiodbRequestScheduler}}.
#'
#' @import methods
#' @include ChildObject.R
#' @export BiodbRequestSchedulerRule
#' @exportClass BiodbRequestSchedulerRule
BiodbRequestSchedulerRule <- methods::setRefClass("BiodbRequestSchedulerRule", contains = "ChildObject", fields = list(.url = "character", .n = "integer", .t = "numeric", .last.time = "list", .n.index = 'integer'))

# Constructor {{{1
################################################################

BiodbRequestSchedulerRule$methods( initialize = function(url, n, t, ...) {

	callSuper(...)

	.self$setUrl(url)
	.self$setFrequence(n = n, t = t)
	.last.time <- list()
	.n.index <- 0
})

# Get URL {{{1
################################################################

BiodbRequestScheduler$methods( getUrl = function() {
	return(.self$.url)
})

# Set URL {{{1
################################################################

BiodbRequestScheduler$methods( setUrl = function(url) {
	.self$.assert.is(url, 'character')
	.url <<- url
})

# URL is similar {{{1
################################################################

BiodbRequestScheduler$methods( urlIsSimilar = function(url) {

	if (length(url) > length(.self$.url))
		similar <- substr(url, 1, length(.self$.url)) == .self$.url
	else
		similar <- substr(.self$.url, 1, length(url)) == url

	return(similar)
})

# Get N {{{1
################################################################

BiodbRequestScheduler$methods( getN = function() {
	return(.self$.n)
})

# Get T {{{1
################################################################

BiodbRequestScheduler$methods( getT = function() {
	return(.self$.t)
})

# Set frequence {{{1
################################################################

BiodbRequestScheduler$methods( setFrequency = function(n, t) {
	.self$.assert.is(n, 'integer')
	.self$.assert.is(t, 'numeric')
	.self$.assert.positive(n)
	.self$.assert.positive(t)

	# Update last time and index
	if (length(.self$.last.time) <= n) {
		.last.time <<- .self$.last.time[((.self$.n.index - (seq(.self$.n) - 1)) %% .self$.n)]
		.n.index <<- length(.self$.last.time)
	}
	else {
		.last.time <<- .self$.last.time[((.self$.n.index - (seq(n) - 1)) %% .self$.n)]
		.n.index <<- n
	}

	# Update frequency
	.n <<- n
	.t <<- t
})

# Wait as needed {{{1
################################################################

BiodbRequestScheduler$methods( wait.as.needed = function() {

	# Do we need to wait?
	if (length(.self$.last.time) == .self$.n) {

		# Look at all "last" times starting from most recent one
		n <- 0
		for (i in ((.self$.n.index - (seq(.self$.n) - 1)) %% .self$.n))
			if (difftime(.self$.last.time[[n]], t, units = 'secs') < .self$.t)
				n <- n + 1
			else
				break

		# Wait
		if (n == .self$.n) {
			n.oldest <- (.self$.n.index + 1) %% .self$.n
			sleep.time <- .self$.t - difftime(Sys.time(), .self$.last.time[[n.oldest]], units = 'secs')
			.self$message('debug', paste('Wait ', sleep.time, ' second(s).', sep = ''))
			Sys.sleep(max(0, sleep.time))
		}
	}

	# Store current time
	.n.index <<- (.self$.n.index + 1) %% n
	.self$.last.time[[.self$.n.index]] <- Sys.time()
})
