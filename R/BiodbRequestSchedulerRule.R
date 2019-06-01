# vi: fdm=marker ts=4 et cc=80

# Class declaration {{{1
################################################################################

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
#' @include BiodbChildObject.R
#' @export BiodbRequestSchedulerRule
#' @exportClass BiodbRequestSchedulerRule
BiodbRequestSchedulerRule <- methods::setRefClass("BiodbRequestSchedulerRule", contains = "BiodbChildObject", fields = list(.host = "character", .n = "integer", .t = "numeric", .last.time = "list", .n.index = 'integer', .conn = 'list'))

# Initialize {{{1
################################################################################

BiodbRequestSchedulerRule$methods( initialize = function(host, n, t, conn, ...) {

    callSuper(...)

    .self$.assert.inherits.from(conn, 'BiodbConn')
    .self$.assert.is(host, 'character')
    .self$.host <- host
    .self$.last.time <- list()
    .self$.n.index <- as.integer(0)
    .self$.conn <- list(conn)
    .self$setFrequency(n = conn$getPropertyValue('scheduler.n'), t = conn$getPropertyValue('scheduler.t'))
})

# Get hostname {{{1
################################################################################

BiodbRequestSchedulerRule$methods( getHost = function() {
    return(.self$.host)
})

# Get N {{{1
################################################################################

BiodbRequestSchedulerRule$methods( getN = function() {
    return(.self$.n)
})

# Get T {{{1
################################################################################

BiodbRequestSchedulerRule$methods( getT = function() {
    return(.self$.t)
})

# Set frequency {{{1
################################################################################

BiodbRequestSchedulerRule$methods( setFrequency = function(n, t) {
    .self$.assert.is(n, 'integer')
    .self$.assert.is(t, c('integer', 'numeric'))
    .self$.assert.positive(n)
    .self$.assert.positive(t)

    # Update last time and index
    if (length(.self$.last.time) >= 1) {
        if (length(.self$.last.time) <= n) {
            .self$.last.time <- .self$.last.time[seq(from = .self$.n.index - 1, to = .self$.n.index - length(.self$.last.time)) %% .self$.n + 1]
            .self$.n.index <- length(.self$.last.time)
        }
        else {
            .self$.last.time <- .self$.last.time[seq(from = .self$.n.index - 1, to = .self$.n.index - n) %% .self$.n + 1]
            .self$.n.index <- n
        }
    }

    # Update frequency
    .self$.n <- n
    .self$.t <- t
})

# Get connectors {{{1
################################################################################

BiodbRequestSchedulerRule$methods( getConnectors = function() {
    return(.self$.conn)
})

# Add connector {{{1
################################################################################

BiodbRequestSchedulerRule$methods( addConnector = function(conn) {
    .self$.assert.inherits.from(conn, 'BiodbConn')

    # Connector already listed?
    if (any(vapply(.self$.conn, function(x) identical(x, conn), FUN.VALUE = TRUE)))
        .self$message('caution', paste0('Connector "', conn$getId(), '" is already listed in rule "', .self$.host, '".'))

    # Add connector
    else {

        .self$.conn <- c(.self$.conn, conn)

        # Update frequency
        .self$recomputeFrequency()
    }
})

# Remove connector {{{1
################################################################################

BiodbRequestSchedulerRule$methods( removeConnector = function(conn) {
    .self$.assert.inherits.from(conn, 'BiodbConn')

    # Connector already listed?
    found.conn <- vapply(.self$.conn, function(x) identical(x, conn), FUN.VALUE = TRUE)
    if ( ! any(found.conn))
        .self$message('caution', paste0('Connector "', conn$getId(), '" is not listed in rule "', .self$.host, '".'))

    # Remove connector
    else {

        # Update frequency

        .self$.conn <- .self$.conn[ ! found.conn]
    }
})

# Recompute frequency {{{1
################################################################################

BiodbRequestSchedulerRule$methods( recomputeFrequency = function() {

    t <- NULL
    n <- NULL

    # Loop on all connectors
    for (conn in .self$.conn) {
        t.conn <- conn$getPropertyValue('scheduler.t')
        n.conn <- conn$getPropertyValue('scheduler.n')
        if (is.null(t) || ((abs(t / n - t.conn / n.conn) < 1e-6 && n.conn < n) || t.conn / n.conn > t / n)) {
            t <- t.conn
            n <- n.conn
        }
    }

    # Set frequency
    .self$setFrequency(n = n, t = t)
})

# Compute sleep time {{{1
################################################################################

BiodbRequestSchedulerRule$methods( computeSleepTime = function(cur.time = NULL) {

    sleep.time <- 0

    if (is.null(cur.time))
        cur.time <-Sys.time()

    # Do we need to wait?
    if (length(.self$.last.time) == .self$.n) {

        # Look at all "last" times starting from most recent one
        n <- 0
        last.time.indices <- seq(from = .self$.n.index - 1, to = .self$.n.index - .self$.n) %% .self$.n + 1
        for (i in last.time.indices)
            if (difftime(.self$.last.time[[i]], cur.time, units = 'secs') < .self$.t)
                n <- n + 1
            else
                break

        # Compute sleep time
        if (n == .self$.n) {
            n.oldest <- .self$.n.index %% .self$.n + 1
            sleep.time <- .self$.t - difftime(cur.time, .self$.last.time[[n.oldest]], units = 'secs')
            sleep.time <- max(0, sleep.time)
        }
    }

    return(sleep.time)
})

# Store current time {{{1
################################################################################

BiodbRequestSchedulerRule$methods( storeCurrentTime = function(cur.time = NULL) {

    if (is.null(cur.time))
        cur.time <-Sys.time()

    .self$.n.index <- as.integer(if (.self$.n.index == .self$.n) 1 else .self$.n.index + 1)
    .self$.last.time[[.self$.n.index]] <- cur.time
})

# Wait as needed {{{1
################################################################################

BiodbRequestSchedulerRule$methods( wait.as.needed = function() {

    # Compute sleep time
    sleep.time <- .self$computeSleepTime()

    # Sleep if needed
    if (sleep.time > 0) {
        .self$message('debug', paste('Wait ', sleep.time, ' second(s).', sep = ''))
        Sys.sleep(sleep.time)
    }

    # Store current time
    .self$storeCurrentTime()
})
# Show {{{1
################################################################################

BiodbRequestSchedulerRule$methods( show = function() {
    cat("Biodb scheduler rule instance.\n")
    cat('  Handle request waiting time for host "', .self$.host, '" for ', length(.self$.conn), " connector(s): ", paste(vapply(.self$.conn, function(x) x$getId(), FUN.VALUE = ''), collapse = ', '), ".\n", sep = '')
    cat('  Parameters are T = ', .self$getT(), ' and N = ', .self$getN(), ".\n", sep = '')
})
