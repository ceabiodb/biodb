#' Class for handling requests.
#'
#' This class handles GET and POST requests, as well as file downloading. Each
#' remote database connection instance creates an instance of
#' \code{BiodbRequestScheduler} for handling database connection. A timer is
#' used to schedule connections, and avoid sending too much requests to the
#' database. This class is not meant to be used directly by the library user.
#' See section Fields for a list of the constructor's parameters.
#'
#' @seealso \code{\link{BiodbRequestSchedulerRule}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Get the scheduler
#' sched <- mybiodb$getRequestScheduler()
#'
#' # Create a request object
#' u <- 'https://www.ebi.ac.uk/webservices/chebi/2.0/test/getCompleteEntity'
#' url <- BiodbUrl$new(url=u)
#' url$setParam('chebiId', 15440)
#' request <- BiodbRequest$new(method='get', url=url)
#'
#' # Send request
#' sched$sendRequest(request)
#'
#' # Terminate instance.
#' mybiodb$terminate()
#' mybiodb <- NULL
#'
#' @import R6
#' @export
BiodbRequestScheduler <- R6::R6Class("BiodbRequestScheduler",

public=list(

#' @description
#' New instance initializer. BiodbRequestScheduler class must not be
#' instantiated direrctly. Instead, use the getRequestScheduler() method from
#' BiodbMain.
#' @param bdb The BiodbMain instance.
#' @return Nothing.
initialize=function(bdb) {

    chk::chk_is(bdb, 'BiodbMain')

    private$bdb <- bdb
    private$connid2rules <- list()
    private$host2rule <- list()
    private$nb.max.tries <- 10L
    private$ssl.verifypeer <- TRUE

    return(invisible(NULL))
},

#' @description
#' Sends a SOAP request to a URL. Returns the string result.
#' @param url The URL to access, as a character string.
#' @param soap.request The XML SOAP request to send, as a character string. 
#' @param soap.action The SOAP action to contact, as a character string.
#' @param encoding The encoding to use.
#' @return The results returned by the contacted server, as a single
#'     string value.
sendSoapRequest=function(url, soap.request, soap.action=NA_character_,
    encoding=integer()) {

    # Prepare request
    header <- c(Accept="text/xml", Accept="multipart/*",
        'Content-Type'="text/xml; charset=utf-8")
    if ( ! is.na(soap.action))
        header <- c(header, SOAPAction=soap.action)

    # Send request
    results <- self$getUrl(url, method='post', header=header,
        body=soap.request, encoding=encoding)

    return(results)
},

#' @description
#' Sends a request, and returns content result.
#' @param request A BiodbRequest instance.
#' @param cache.read If set to TRUE, the cache system will be used. In case the
#' same request has already been run and its results saved into the cache, then
#' the request is not run again, the targeted server not contacted, and the
#' results are directly loaded from the cache system.
#' @return The results returned by the contacted server, as a single string
#' value.
sendRequest=function(request, cache.read=TRUE) {

    content <- NA_character_
    cch <- private$bdb$getPersistentCache()
    cfg <- private$bdb$getConfig()

    # Get rule
    rule <- self$findRule(request$getUrl())

    # Log URL
    logDebug0("Getting content of ", request$getMethod(), " URL request \"",
        request$getUrl()$toString(encode=FALSE), "\".")

    # Try to get query result from cache
    request.key <- request$getUniqueKey()
    conn <- request$getConn()
    if (cache.read && cfg$isEnabled('cache.system')
        && cfg$get('cache.all.requests')
        && ! is.null(conn)
        && cch$fileExists(conn$getCacheId(), name=request.key, ext='content')) {
        logDebug("Loading content of request from cache.")
        content <- cch$loadFileContent(conn$getCacheId(),
            name=request.key, ext='content', output.vector=TRUE)
    }

    if (is.na(content)) {

        # Check if in offline mode
        private$checkOfflineMode()

        content <- private$doSendRequestLoop(request=request, rule=rule)

        # Save content to cache
        if ( ! is.na(content) && cfg$isEnabled('cache.system')
            && ! is.null(conn)
            && cfg$get('cache.all.requests')) {
            logDebug("Saving content of request to cache.")
            cch$saveContentToFile(content, cache.id=conn$getCacheId(),
                name=request.key, ext='content')
            cch$saveContentToFile(request$toString(),
                cache.id=conn$getCacheId(), name=request.key, ext='request')
        }
    }

    return(content)
},

#' @description
#' Downloads the content of a URL and save it into the specified
#'     destination file.
#' @param url The URL to access, as a BiodbUrl object.
#' @param dest.file A path to a destination file.
#' @return Nothing.
downloadFile=function(url, dest.file) {

    # Get rule
    rule <- self$findRule(url)

    # Wait required time between two requests
    rule$waitAsNeeded()

    # Convert URL to string
    url <- url$toString()

    # Make sure path exists
    path <- dirname(dest.file)
    if ( ! dir.exists(path))
        dir.create(path, recursive=TRUE)

    # Download
    logDebug('Downloading file "%s".', url)
    cfg <- private$bdb$getConfig()
    options(HTTPUserAgent=cfg$get('useragent'),
        timeout=cfg$get('dwnld.timeout'))
    utils::download.file(url=url, destfile=dest.file, mode='wb',
        method='auto', cacheOK=FALSE, quiet=FALSE)
    # TODO Add a biodb option for "quiet"?

    return(invisible(NULL))
},

#' @description
#' Call back function called when connector URLs are changed.
#' @param conn The connector instance for which the URLs were changed.
#' @return Nothing.
notifyConnUrlsUpdated=function(conn) {

    self$unregisterConnector(conn)
    self$registerConnector(conn)

    return(invisible(NULL))
},

#' @description
#' Call back function called when connector T and N parameters (frequency) are
#' changed.
#' @param conn The connector instance for which the frequency were changed.
#' @return Nothing.
notifyConnSchedulerFrequencyUpdated=function(conn) {

    logDebug("Frequency changed for connector %s.", conn$getId())
    # Is connector not registered?
    if ( ! conn$getId() %in% names(private$connid2rules))
        warn('Connector "%s" has never been registered.', conn$getId())

    # Update frequency
    else {
        for (rule in private$connid2rules[[conn$getId()]])
            rule$recomputeFrequency()
    }

    return(invisible(NULL))
},

#' @description
#' Builds a URL object, using a base URL and parameters to be passed.
#' @param url A URL string.
#' @param params A list of URL parameters.
#' @return A BiodUrl object.
getUrlString=function(url, params=list()) {
    lifecycle::deprecate_soft('1.0.0', 'getUrlString()', "BiodbUrl::toString()")

    url <- BiodbUrl$new(url=url, params=params)$toString(encode=FALSE)

    return(url)
},

#' @description
#' Sends a request and get the result.
#' @param url A URL string.
#' @param params A list of URL parameters.
#' @param method The method to use. Either 'get' or 'post'.
#' @param header The header to send.
#' @param body The body to send.
#' @param encoding The encoding to use.
#' @return The results of the request.
getUrl=function(url, params=list(), method=c('get', 'post'), header=character(),
    body=character(), encoding=integer()) {

    lifecycle::deprecate_warn('1.0.0', 'getUrl()',
        "BiodbRequestScheduler::sendRequest()")

    method <- match.arg(method)

    request <- BiodbRequest$new(url=BiodbUrl$new(url=url, params=params),
        method=method, header=header, body=body, encoding=encoding)

    return(self$sendRequest(request))
},

#' @description
#' Searches for a rule by host name. 
#' @param url     The host URL.
#' @param create  Sets to TRUE to create a rule when none exists.
#' @return A BiodbRequestSchedulerRule object.
findRule=function(url, create=TRUE) {

    chk::chk_not_null(url)
    if ( ! is(url, 'BiodbUrl')) {
        chk::chk_string(url)
        url <- BiodbUrl$new(url=url)
    }
    domain <- url$getDomain()

    # Rule does not exist
    if (create && ! domain %in% names(private$host2rule)) {
        logInfo0('No rule exists for domain "', domain,
            '". Creating a default one.')
        rule <- BiodbRequestSchedulerRule$new(host=domain, conn=NULL)
        private$host2rule[[domain]] <- rule
    }

    return(private$host2rule[[domain]])
},

#' @description
#' Gets the rules associates with a connector.
#' @param conn A valid connector object.
#' @return A list of rules.
getConnectorRules=function(conn) {
    chk::chk_is(conn, 'BiodbConn')
    return(private$connid2rules[[conn$getId()]])
},

#' @description
#' Registers a new connector with the scheduler.
#' @param conn A valid connector object.
#' @return Nothing.
registerConnector=function(conn) {

    logDebug('Register connector %s.', conn$getId())

    # Is connector already registered?
    if (conn$getId() %in% names(private$connid2rules))
        warn('Connector "%s" has already been registered.', conn$getId())

    # Add connector
    else {
        # Register as observer
        conn$.__enclos_env__$private$registerObserver(self)

        # Add connector
        private$addConnectorRules(conn)
    }
    
    return(invisible(NULL))
},

#' @description
#' Unregisters a connector from this scheduler.
#' @param conn A valid connector object.
#' @return Nothing.
unregisterConnector=function(conn) {

    # Is connector not registered?
    if ( ! conn$getId() %in% names(private$connid2rules))
        warn('Connector "%s" has never been registered.', conn$getId())

    # Unregister connector
    else {
        # Unregister as observer
        conn$.__enclos_env__$private$unregisterObserver(self)

        # Remove connector
        private$removeConnectorRules(conn)
    }
    
    return(invisible(NULL))
},

#' @description
#' Gets all defined rules.
#' @return The list of all rules.
getAllRules=function() {
    return(private$host2rule)
}
),

private=list(
    host2rule=NULL,
    connid2rules=NULL,
    bdb=NULL,
    nb.max.tries=NULL,
    ssl.verifypeer=NULL,

checkOfflineMode=function() {

    if (private$bdb$getConfig()$isEnabled('offline'))
        error("Offline mode is enabled. All connections are forbidden.")
},

addConnectorRules=function(conn) {

    private$connid2rules[[conn$getId()]] <- list()

    # Loop on all connector URLs
    for (url in conn$getPropertyValue('urls')) {

        # Check if a rule already exists
        rule <- self$findRule(url, create=FALSE)

        # No rule exists => create new one
        if (is.null(rule)) {
            host <- BiodbUrl$new(url=url)$getDomain()
            logDebug0('Create new rule for URL "', host,'" of connector "',
                conn$getId(), '".')
            rule <- BiodbRequestSchedulerRule$new(host=host, conn=conn)
            private$host2rule[[rule$getHost()]] <- rule
        }

        # A rule with the same host already exists, add connector to it
        else
            rule$addConnector(conn)

        # Add rule
        rules <- private$connid2rules[[conn$getId()]]
        fct <- function(x) identical(rule, x)
        if ( ! any(vapply(rules, fct, FUN.VALUE=TRUE)))
            private$connid2rules[[conn$getId()]] <- c(rules, rule)
    }

    return(invisible(NULL))
},

removeConnectorRules=function(conn) {

    # Get rules
    rules <- private$connid2rules[[conn$getId()]]

    # Loop on connector rules
    for (rule in rules) {

        if (length(rule$getConnectors()) <= 1)
            private$host2rule[[rule$getHost()]] <- NULL
        else
            rule$removeConnector(conn)
    }
    
    # Remove connector
    private$connid2rules[[conn$getId()]] <- NULL
},

processRequestErrors=function(content, hdr, err_msg, retry) {

    # Recoverable HTTP errors
    lst <- c(.HTTP.STATUS.NOT.FOUND, .HTTP.STATUS.REQUEST.TIMEOUT,
        .HTTP.STATUS.INTERNAL.SERVER.ERROR, .HTTP.STATUS.FOUND,
        .HTTP.STATUS.SERVICE.UNAVAILABLE) 
    if ( ! is.null(hdr) && hdr$status %in% lst) {
        err_msg <- paste0("HTTP error ", hdr$status," (\"", hdr$statusMessage,
            "\").")
        if ('Retry-After' %in% names(hdr))
            err_msg <- paste(err_msg, "Retry after", hdr[['Retry-After']],
                ".")
        if ("location" %in% names(hdr))
            err_msg <- paste(err_msg, "Redirect location to", hdr[["location"]])
        retry <- TRUE
    }

    # Other HTTP errors
    if (is.null(err_msg) && ! is.null(hdr) && hdr$status != .HTTP.STATUS.OK) {
        err_msg <- paste0("Unrecoverable HTTP error ", hdr$status," (\"",
            hdr$statusMessage, "\").")
        if ('Retry-After' %in% names(hdr))
            err_msg <- paste0(err_msg, " Retry after ", hdr[['Retry-After']],
                ".")
        content <- NA_character_
        retry <- FALSE
    }

    # Proxy server error
    # This happens sometime with NCBI CCDS server.
    if (is.null(err_msg) && ! is.null(content) && ! is.na(content)
        && length(grep('The proxy server could not handle the request',
        unname(content))) > 0) {
        logDebug('Found proxy error message in content.')
        err_msg <- "Error between the proxy and the main server."
        content <- NA_character_
        retry <- FALSE
    }

    return(list(retry=retry, err_msg=err_msg))
},

doSendRequestOnce=function(request) {

    content <- NA_character_
    err_msg <- NULL
    retry <- FALSE
    cfg <- private$bdb$getConfig()

    # Build options
    opts <- request$getCurlOptions(useragent=cfg$get('useragent'))
    logTrace('Sent URL is "%s".', request$getUrl()$toString())

    # Create HTTP header object (to receive HTTP information from server).
    header <- RCurl::basicHeaderGatherer()

    curl.error <- NULL
    header$reset()
    content <- tryCatch(expr={
            if (request$getMethod() == 'get')
                RCurl::getURL(request$getUrl()$toString(), .opts=opts,
                    ssl.verifypeer=private$ssl.verifypeer,
                    .encoding=request$getEncoding(),
                    headerfunction=header$update)
            else
                RCurl::postForm(request$getUrl()$toString(), .opts=opts,
                    .encoding=request$getEncoding(),
                    headerfunction=header$update)
            },
        PEER_FAILED_VERIFICATION=function(err) { retry=TRUE ; curl.error=err },
        GenericCurlError=function(err) { retry=TRUE ; curl.error=err },
        error <-function(err) { retry=FALSE ; curl.error=err })

    # RCurl error
    if ( ! is.null(curl.error))
        err_msg <- paste0("RCurl error: ", curl.error)

    # Get header information sent by server
    hdr <- NULL
    if (is.null(err_msg)) {
        # We want to catch "<simpleWarning in max(i): no non-missing arguments
        # to max; returning -Inf>".
        hdr <- tryCatch(expr=as.list(header$value()),
            warning=function(w) w,
            error=function(e) e)

        if (methods::is(hdr, 'simpleError')
            || methods::is(hdr, 'simpleWarning')) {
            err_msg <- paste0('Error while retrieving HTTP header: ', hdr, '.')
            hdr <- NULL
            retry <- TRUE
        }

        if ( ! is.null(hdr)) {
            hdr$status <- as.integer(hdr$status)
            if (hdr$status == 0) {
                hdr <- NULL
                err_msg <- "Cannot find status info in HTTP header."
                retry <- TRUE
            }
        }
    }

    res <- private$processRequestErrors(content=content, hdr=hdr,
        err_msg=err_msg, retry=retry)

    return(list(content=content, err_msg=res$err_msg, retry=res$retry))
},

doSendRequestLoop=function(request, rule) {

    content <- NA_character_

    # Enter query loop
    i <- 0
    retry <- TRUE
    while (retry && i < private$nb.max.tries) {

        # Increment try number
        i <- i + 1

        # Print debug information about header and body
        logDebug('Request header is: "%s".', request$getHeaderAsSingleString())
        logDebug('Request body is "%s".',
            paste(request$getBody(), collapse=', '))

        # Wait required time between two requests
        rule$waitAsNeeded()

        # Send request
        res <- private$doSendRequestOnce(request=request)
        retry <- res$retry

        # Print connection error message
        if ( ! is.null(res$err_msg)) {
            if (retry) {
                m <- paste0(" When contacting URL \"",
                    request$getUrl()$toString(),
                    "\". Retrying connection to server...")
                res$err_msg=paste0(res$err_msg, m)
            }
            logInfo(res$err_msg)
        }
        else
            content <- res$content
    }

    return(content)
}
))
