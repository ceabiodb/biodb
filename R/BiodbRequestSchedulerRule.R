#' Scheduler rule class.
#'
#' This class represents a rule for the request scheduler.
#'
#' @seealso \code{\link{BiodbRequestScheduler}}.
#'
#' @import R6
BiodbRequestSchedulerRule <- R6::R6Class("BiodbRequestSchedulerRule",

public=list(

#' @description
#' Constructor.
#' @param host The web host for which this rules is applicable.
#' @param conn The connector instance that is concerned by this rule.
initialize=function(host, conn=NULL) {

    chk::chk_character(host)
    private$host <- host
    private$last.time <- list()
    private$n.index <- 0L
    if ( ! is.null(conn)) {
        chk::chk_is(conn, 'BiodbConn')
        private$conn <- list(conn)
        self$setFrequency(n=conn$getPropertyValue('scheduler.n'),
            t=conn$getPropertyValue('scheduler.t'))
    }
    else {
        private$conn <- list()
        self$setFrequency(n=3L, t=1L)
    }
},

#' @description
#' Gets host.
#' @return Returns the host.
getHost=function() {

    return(private$host)
},

#' @description
#' Gets N value. The number of connections allowed during a period of
#'     T seconds.
#' @return Returns N as an integer.
getN=function() {

    return(private$n)
},

#' @description
#' Gets T value. The number of seconds during which N connections
#' are allowed.
#' @return Returns T as a numeric.
getT=function() {

    return(private$t)
},

#' @description
#' Sets both N and T.
#' @param n The number of connections allowed during a period of t seconds,
#' as an integer.
#' @param t The number of seconds during which n connections are allowed, as a
#' numeric value.
#' @return None.
setFrequency=function(n, t) {

    chk::chk_whole_number(n)
    chk::chk_number(t)
    chk::chk_gte(n, 1)
    chk::chk_gt(t, 0)

    # Update last time and index
    if (length(private$last.time) >= 1) {
        ni <- private$n.index
        x <- min(length(private$last.time), n)
        i <- seq(from=ni-1, to=ni-x) %% private$n + 1
        private$last.time <- private$last.time[i]
        private$n.index <- x
    }

    # Update frequency
    private$n <- n
    private$t <- t
},

#' @description
#' Gets connectors associaated with this rule.
#' @return A list of BiodbConn objects.
getConnectors=function() {

    return(private$conn)
},

#' @description
#' Associate a connector with this rule.
#' @param conn A BiodbConn object.
#' @return None.
addConnector=function(conn) {

    chk::chk_is(conn, 'BiodbConn')

    # Connector already listed?
    if (any(vapply(private$conn, function(x) identical(x, conn),
        FUN.VALUE=TRUE)))
        logDebug0('Connector "', conn$getId(),
            '" is already listed in rule "', private$host, '".')

    # Add connector
    else {

        private$conn <- c(private$conn, conn)

        # Update frequency
        self$recomputeFrequency()
    }
},

#' @description
#' Disassociate a connector from this rule.
#' @param conn A BiodbConn instance.
#' @return None.
removeConnector=function(conn) {

    chk::chk_is(conn, 'BiodbConn')

    # Connector already listed?
    found.conn <- vapply(private$conn, function(x) identical(x, conn),
        FUN.VALUE=TRUE)
    if ( ! any(found.conn))
        warn('Connector "%s" is not listed in rule "%s".', conn$getId(),
            private$host)

    # Remove connector
    else {

        # Update frequency

        private$conn <- private$conn[ ! found.conn]
    }
},

#' @description
#' Displays information about this instance.
#' @return None.
print=function() {

    cat("Biodb scheduler rule instance.\n")
    conlst <- paste(vapply(private$conn, function(x) x$getId(), FUN.VALUE=''),
        collapse=', ')
    cat('  Handle request waiting time for host "', private$host, '" for ',
        length(private$conn), " connector(s): ", conlst, ".\n", sep='')
    cat('  Parameters are T=', self$getT(), ' and N=', self$getN(), ".\n",
        sep='')
}

#' @description
#' Wait (sleep) until a new request is allowed.
#' @return Nothing.
,waitAsNeeded=function() {

    # Compute sleep time
    sleep.time <-self$computeSleepTime()

    # Sleep if needed
    if (sleep.time > 0) {
        logDebug('Wait %g second(s).', sleep.time)
        Sys.sleep(sleep.time)
    }

    # Store current time
    self$storeCurrentTime()
}

#' @description
#' Recompute frequency from submitted N and T values. 
#' @return Nothing.
,recomputeFrequency=function() {

    t <- NULL
    n <- NULL

    # Loop on all connectors
    for (conn in private$conn) {
        t.conn <- conn$getPropertyValue('scheduler.t')
        n.conn <- conn$getPropertyValue('scheduler.n')
        if (is.null(t) || ((abs(t / n - t.conn / n.conn) < 1e-6 && n.conn < n)
            || t.conn / n.conn > t / n)) {
            t <- t.conn
            n <- n.conn
        }
    }

    # Set frequency
    self$setFrequency(n=n, t=t)
}

#' @description
#' Compute the needed sleep time to wait until a new request is allowed,
#' starting from the submitted time.
#' @param cur.time Time from which to compute needed sleep time.
#' @return The needed sleep time in seconds.
,computeSleepTime=function(cur.time=Sys.time()) {

    sleep.time <- 0

    # Do we need to wait?
    if (length(private$last.time) == private$n) {

        # Look at all "last" times starting from most recent one
        n <- 0
        last.time.indices <- seq(from=private$n.index - 1,
            to=private$n.index - private$n) %% private$n + 1
        for (i in last.time.indices) {
            dt <- difftime(private$last.time[[i]], cur.time, units='secs')
            if (dt < private$t)
                n <- n + 1
            else
                break
        }

        # Compute sleep time
        if (n == private$n) {
            n.oldest <- private$n.index %% private$n + 1
            sleep.time <- private$t - difftime(cur.time,
                private$last.time[[n.oldest]], units='secs')
            sleep.time <- max(0, sleep.time)
        }
    }

    return(sleep.time)
}

#' @description
#' Stores the current time.
#' @param cur.time The current time.
#' @return Nothing.
,storeCurrentTime=function(cur.time=Sys.time()) {

    private$n.index <- as.integer(if (private$n.index == private$n) 1
        else private$n.index + 1)
    private$last.time[[private$n.index]] <- cur.time
}
),

private=list(
    host=NULL,
    n=NULL,
    t=NULL,
    conn=NULL,
    n.index=NULL,
    last.time=NULL
))
