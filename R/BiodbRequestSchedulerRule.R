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
BiodbRequestSchedulerRule <- methods::setRefClass("BiodbRequestSchedulerRule", contains = "ChildObject", fields = list(.host = "character", .n = "integer", .t = "numeric", .last.time = "list", .n.index = 'integer', .conn = 'list'))

# Constructor {{{1
################################################################

BiodbRequestSchedulerRule$methods( initialize = function(host, n, t, conn, ...) {

	callSuper(...)

	.self$.assert.is(conn, 'BiodbConn')
	.self$.assert.is(host, 'character')
	.host <<- host
	.last.time <- list()
	.n.index <- 0
	.conn <- list(conn)
	.self$setFrequency(n = conn$getSchedulerNParam(), t = conn$getSchedulerTParam())
})

# Get hostname {{{1
################################################################

BiodbRequestScheduler$methods( getHost = function() {
	return(.self$.host)
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

# Set frequency {{{1
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

# Get connector {{{1
################################################################

BiodbRequestScheduler$methods( getConnectors = function() {
	return(.self$.conn)
})

# Add connector {{{1
################################################################

BiodbRequestScheduler$methods( addConnector = function(conn, host) {
	.self$.assert.is(conn, 'BiodbConn')

	# Connector already listed?
	if (any(vapply(.self$.conn, function(x) identical(x, conn), FUN.VALUE = TRUE)))
		.self$message('caution', paste0('Connector "', conn$getId(), '" is already listed in rule "' .self$.host, '".'))

	# Add connector
	else {

		# Update frequency
		t <- conn$getSchedulerTParam()
		n <- conn$getSchedulerNParam()
		if ((abs(.self$getT() / .self$getN() - t / n) < 0.1 && .self$getN() > n) # equivalent rule
			|| .self$getT() / .self() > t / n) {
			.self$message('debug', paste0('Replacing rule frequency ', .self$getN(),' / ', .self$getT(),' by ', n, ' / ', t, ', for connector "', conn$getId(), '"'))
			.self$setFrequency(n = n, t = t)
		}

		.conn <<- c(.self$.conn, conn)
	}
})

# Remove connector {{{1
################################################################

BiodbRequestScheduler$methods( removeConnector = function(conn) {
	.self$.assert.is(conn, 'BiodbConn')

	# Connector already listed?
	found.conn <- vapply(.self$.conn, function(x) identical(x, conn), FUN.VALUE = TRUE))
	if ( ! any(found.conn))
		.self$message('caution', paste0('Connector "', conn$getId(), '" is not listed in rule "' .self$.host, '".'))

	# Remove connector
	else {

		# Update frequency

		.conn <<- .self$.conn[ ! found.conn]
	}
})

# Recompute frequency {{{1
################################################################

BiodbRequestScheduler$methods( recomputeFrequency = function() {
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
