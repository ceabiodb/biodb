# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbRequestSchedulerRule {{{1
################################################################################

# Declaration {{{2
################################################################################

#' Scheduler rule class.
#'
#' This class represents a rule for the request scheduler.
#'
#' The constructor takes the following arguments:
#'
#' host: The web host for which this rules is applicable.
#'
#' n: The number of connections allowed during a period of t seconds.
#'
#' t: The number of seconds during which n connections are allowed.
#'
#' conn: The connector instance that is concerned by this rule.
#'
#' @seealso \code{\link{BiodbRequestScheduler}}.
#'
#' @import methods
#' @include BiodbChildObject.R
#' @export BiodbRequestSchedulerRule
#' @exportClass BiodbRequestSchedulerRule
BiodbRequestSchedulerRule <- methods::setRefClass("BiodbRequestSchedulerRule",
    contains="BiodbChildObject",
    fields=list(
        .host="character",
        .n="integer",
        .t="numeric",
        .last.time="list",
        .n.index='integer',
        .conn='list'
     ),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(host, n, t, conn, ...) {

    callSuper(...)

    .self$.assertInheritsFrom(conn, 'BiodbConn')
    .self$.assertIs(host, 'character')
    .self$.host <- host
    .self$.last.time <- list()
    .self$.n.index <- as.integer(0)
    .self$.conn <- list(conn)
    .self$setFrequency(n=conn$getPropertyValue('scheduler.n'),
                       t=conn$getPropertyValue('scheduler.t'))
},

# Get hostname {{{3
################################################################################

getHost=function() {
    ":\n\nGets host.
    \nReturned value: Returns the host.
    "
    
    return(.self$.host)
},

# Get N {{{3
################################################################################

getN=function() {
    ":\n\nGets N value. The number of connections allowed during a period of
    T seconds.
    \nReturned value: Returns N as an integer.
    "
    
    return(.self$.n)
},

# Get T {{{3
################################################################################

getT=function() {
    ":\n\nGets T value. The number of seconds during which N connections
    are allowed.
    \nReturned value: Returns T as a numeric.
    "
    
    return(.self$.t)
},

# Set frequency {{{3
################################################################################

setFrequency=function(n, t) {
    ":\n\nSets both N and T.
    \nn: The number of connections allowed during a period of t seconds,
    as an integer.
    \nt: The number of seconds during which n connections are allowed, as a
    numeric value.
    \nReturned value: None.
    "

    .self$.assertIs(n, 'integer')
    .self$.assertIs(t, c('integer', 'numeric'))
    .self$.assertPositive(n)
    .self$.assertPositive(t)

    # Update last time and index
    if (length(.self$.last.time) >= 1) {
        ni <- .self$.n.index
        x <- min(length(.self$.last.time), n)
        i <- seq(from=ni-1, to=ni-x) %% .self$.n + 1
        .self$.last.time <- .self$.last.time[i]
        .self$.n.index <- x
    }

    # Update frequency
    .self$.n <- n
    .self$.t <- t
},

# Get connectors {{{3
################################################################################

getConnectors=function() {
    ":\n\nGets connectors associaated with this rule.
    \nReturned value: A list of BiodbConn objects.
    "

    return(.self$.conn)
},

# Add connector {{{3
################################################################################

addConnector=function(conn) {
    ":\n\nAssociate a connector with this rule.
    \nconn: A BiodbConn object.
    \nReturned value: None.
    "

    .self$.assertInheritsFrom(conn, 'BiodbConn')

    # Connector already listed?
    if (any(vapply(.self$.conn, function(x) identical(x, conn),
                   FUN.VALUE=TRUE)))
        .self$debug('Connector "', conn$getId(),
                    '" is already listed in rule "', .self$.host, '".')

    # Add connector
    else {

        .self$.conn <- c(.self$.conn, conn)

        # Update frequency
        .self$.recomputeFrequency()
    }
},

# Remove connector {{{3
################################################################################

removeConnector=function(conn) {
    ":\n\nDisassociate a connector from this rule.
    \nconn: A BiodbConn instance.
    \nReturned value: None.
    "

    .self$.assertInheritsFrom(conn, 'BiodbConn')

    # Connector already listed?
    found.conn <- vapply(.self$.conn, function(x) identical(x, conn),
                         FUN.VALUE=TRUE)
    if ( ! any(found.conn))
        .self$caution('Connector "', conn$getId(), '" is not listed in rule "',
                      .self$.host, '".')

    # Remove connector
    else {

        # Update frequency

        .self$.conn <- .self$.conn[ ! found.conn]
    }
},

# Show {{{3
################################################################################

show=function() {
    ":\n\nDisplays information about this instance.
    \nReturned value: None.
    "

    cat("Biodb scheduler rule instance.\n")
    conlst <- paste(vapply(.self$.conn, function(x) x$getId(), FUN.VALUE=''),
                    collapse=', ')
    cat('  Handle request waiting time for host "', .self$.host, '" for ',
        length(.self$.conn), " connector(s): ", conlst, ".\n", sep='')
    cat('  Parameters are T=', .self$getT(), ' and N=', .self$getN(), ".\n",
        sep='')
},

# Private methods {{{2
################################################################################

# Store current time {{{3
################################################################################

.storeCurrentTime=function(cur.time=NULL) {

    if (is.null(cur.time))
        cur.time <-Sys.time()

    .self$.n.index <- as.integer(if (.self$.n.index == .self$.n) 1
                                 else .self$.n.index + 1)
    .self$.last.time[[.self$.n.index]] <- cur.time
},

# Compute sleep time {{{3
################################################################################

.computeSleepTime=function(cur.time=NULL) {

    sleep.time <- 0

    if (is.null(cur.time))
        cur.time <-Sys.time()

    # Do we need to wait?
    if (length(.self$.last.time) == .self$.n) {

        # Look at all "last" times starting from most recent one
        n <- 0
        last.time.indices <- seq(from=.self$.n.index - 1,
                                 to=.self$.n.index - .self$.n) %% .self$.n + 1
        for (i in last.time.indices) {
            dt <- difftime(.self$.last.time[[i]], cur.time, units='secs')
            if (dt < .self$.t)
                n <- n + 1
            else
                break
        }

        # Compute sleep time
        if (n == .self$.n) {
            n.oldest <- .self$.n.index %% .self$.n + 1
            sleep.time <- .self$.t - difftime(cur.time,
                                              .self$.last.time[[n.oldest]],
                                              units='secs')
            sleep.time <- max(0, sleep.time)
        }
    }

    return(sleep.time)
},

# Wait as needed {{{3
################################################################################

.waitAsNeeded=function() {

    # Compute sleep time
    sleep.time <- .self$.computeSleepTime()

    # Sleep if needed
    if (sleep.time > 0) {
        .self$debug('Wait ', sleep.time, ' second(s).')
        Sys.sleep(sleep.time)
    }

    # Store current time
    .self$.storeCurrentTime()
},

# Recompute frequency {{{3
################################################################################

.recomputeFrequency=function() {

    t <- NULL
    n <- NULL

    # Loop on all connectors
    for (conn in .self$.conn) {
        t.conn <- conn$getPropertyValue('scheduler.t')
        n.conn <- conn$getPropertyValue('scheduler.n')
        if (is.null(t) || ((abs(t / n - t.conn / n.conn) < 1e-6 && n.conn < n)
                           || t.conn / n.conn > t / n)) {
            t <- t.conn
            n <- n.conn
        }
    }

    # Set frequency
    .self$setFrequency(n=n, t=t)
}

))
