# vi: fdm=marker ts=4 et cc=80 tw=80

# PeakforestConn {{{1
################################################################################

#' @include BiodbRemotedbConn.R
PeakforestConn <- methods::setRefClass("PeakforestConn",
    contains="BiodbRemotedbConn",
    fields=list(
        .db.name='character'
    ),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(db.name, ...) {

    # Call mother class constructor
    callSuper(...)
    .self$.abstractClass('PeakforestConn')

    # Set db name
    .self$.db.name <- db.name

    # Check token
    if (is.na(.self$getPropertyValue('token')))
        .self$caution("Peakforest requires a token to function correctly.")
},

# Check if error {{{3
################################################################################

.checkIfError=function(content) {

    if (length(grep('^<!DOCTYPE HTML ', content)) > 0) {
        .self$message('debug', paste("Peakforest returned error: ", content))
        return(TRUE)
    }

    if (length(grep('^<html>.*Apache Tomcat.*Error report', content)) > 0)
        .self$message('error', paste("Peakforest connection error: ", content))

    return(FALSE)
},

# Get entry content from database {{{3
################################################################################

getEntryContentFromDb=function(entry.id) {
    
    # Initialize contents to return
    content <- rep(NA_character_, length(entry.id))

    # Get URLs
    urls <- .self$getEntryContentRequest(entry.id, max.length=2048)

    # Send request
    f <- function(url) .self$getBiodb()$getRequestScheduler()$getUrl(url)
    jsonstr <- vapply(urls, f, FUN.VALUE='')

    # Get directly one JSON string for each ID
    if (length(jsonstr) == length(entry.id)) {
        for (i in seq_along(jsonstr)) {
            if (is.na(jsonstr[[i]]))
                json <- NULL
            else
                json <- jsonlite::fromJSON(jsonstr[[i]],
                                           simplifyDataFrame=FALSE)
            if (is.null(json))
                next

            # XXX TODO What do those lines? In which context are they run?
            # XXX The loop ends in all cases!
            if (methods::is(json, 'list') && ! is.null(names(json))) {
                content=jsonstr
                return(content)
            }
            else
                break
        }
    }

    # Parse all JSON strings
    for (single.jsonstr in jsonstr) {

        if (.self$.checkIfError(single.jsonstr))
            break
        
        if (is.na(single.jsonstr))
            json <- NULL 
        else
            json <- jsonlite::fromJSON(single.jsonstr, simplifyDataFrame=FALSE)

        if ( ! is.null(json)) {
            if (methods::is(json, 'list') && is.null(names(json))) {
                null <- vapply(json, is.null, FUN.VALUE=TRUE)
                f <- function(x) as.character(x$id)
                json.ids <- vapply(json[ ! null], f, FUN.VALUE='')
                f <- function(x) jsonlite::toJSON(x, pretty=TRUE,
                                                  digits=NA_integer_)
                c <- vapply(json[ ! null], f, FUN.VALUE='')
                content[entry.id %in% json.ids] <- c
            }
        }
    }

    return(content)
},

# Get nb entries {{{3
################################################################################

getNbEntries=function(count=FALSE) {
    return(.self$wsAllCount(retfmt='parsed'))
},

# Web service search {{{3
################################################################################

wsSearch=function(term, max=NA_integer_, retfmt=c('plain', 'request', 'parsed',
                                                  'ids')) {

    retfmt <- match.arg(retfmt)

    # Build request
    params <- c(token=.self$getPropertyValue('token'))
    if ( ! is.na(max))
        params <- c(params, max=max)
    u <- c(.self$getPropValSlot('urls', 'ws.url'), 'search', .self$.db.name,
           term)
    url <- BiodbUrl(url=u, params=params)
    request <- BiodbRequest(method='get', url=url)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse
    if (retfmt != 'plain') {

        # Check JSON
        if ( ! jsonlite::validate(results))
            .self$error("Invalid JSON returned by server.")

        # Parse results
        results <- jsonlite::fromJSON(results, simplifyDataFrame=FALSE)

        # Extract IDs
        if (retfmt == 'ids') {
            if ('compoundNames' %in% names(results)) {
                f <- function(x) as.character(x$compound$id)
                results <- vapply(results$compoundNames, f, FUN.VALUE='')
            }
            else
                .self$error('Could find "compoundNames" field inside returned',
                            ' JSON.')
        }
    }

    return(results)
},

# Web service all.count {{{3
################################################################################

wsAllCount=function(retfmt=c('plain', 'request', 'parsed')) {

    retfmt <- match.arg(retfmt)

    # Build request
    params <- c(token=.self$getPropertyValue('token'))
    u <- c(.self$getPropValSlot('urls', 'ws.url'), .self$.db.name, 'all',
           'count')
    url <- BiodbUrl(url=u, params=params)
    request <- BiodbRequest(method='get', url=url)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse
    if (retfmt != 'plain') {

        # Parse integer
        results <- as.integer(results)
    }

    return(results)
},

# Web service all.ids {{{3
################################################################################

wsAllIds=function(retfmt=c('plain', 'request', 'parsed', 'ids')) {

    retfmt <- match.arg(retfmt)

    # Build request
    params <- c(token=.self$getPropertyValue('token'))
    u <- c(.self$getPropValSlot('urls', 'ws.url'), .self$.db.name, 'all', 'ids')
    url <- BiodbUrl(url=u, params=params)
    request <- BiodbRequest(method='get', url=url)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)
    .self$.checkIfError(results)

    # Parse
    if (retfmt != 'plain') {

        # Check JSON
        if ( ! jsonlite::validate(results))
            .self$error("Invalid JSON returned by server.")

        # Parse JSON
        results <- jsonlite::fromJSON(results, simplifyDataFrame=FALSE)

        # extract IDs
        if (retfmt == 'ids')
            results <- as.character(results)
    }

    return(results)
},

# Private methods {{{2
################################################################################

# Get entry ids {{{3
################################################################################

.doGetEntryIds=function(max.results=NA_integer_) {

    # Get all IDs
    ids <- .self$wsAllIds(retfmt='ids')

    return(ids)
}

))
