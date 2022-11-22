#' Get a URL content.
#'
#' @param u The URL as a single character value.
#' @param binary Set to TRUE if the content to be retrieved is binary.
#' @return The content, as a single character value.
getUrlContent <- function(u, binary=FALSE) {
    chk::chk_string(u)
    chk::chk_flag(binary)

    opts <- makeRCurlOptions()
    if (RCurl::url.exists(u, .opts=opts)) {
        logDebug("Use RCurl for downloading content of URL %s.", u)
        content <- getRCurlContent(u, opts=opts, binary=binary)
    } else {
        logDebug("Use base::url() for downloading content of URL %s.", u)
        content <- getBaseUrlContent(u, binary=binary)
    }

    return(content)
}

#' Send a request and get results.
#'
#' @param request A BiodbRequest object.
#' @param useragent The user agent identification.
#' @param ssl.verifypeer Set to TRUE to enable SSL verify peer.
#' @return A RequestResult object.
getUrlRequestResult <- function(request, useragent=NULL, ssl.verifypeer=TRUE) {
    chk::chk_is(request, 'BiodbRequest')
    chk::chk_null_or(useragent, vld=chk::vld_string)
    chk::chk_flag(ssl.verifypeer)

    # Tests first if URL exists, since it may occur that RCurl does not
    # see a valid URL as in the case of UniProt server on Windows.
    # We want to catch the following error:
    # <simpleWarning in max(i): no non-missing arguments to max;
    #     returning -Inf>
    #    
    # This error happens on Windows when downloading from UniProt using
    # RCurl:
    # https://www.uniprot.org/uniprot/?query=&columns=id&format=tab&limit=2
    #
    # More precisely the original error is:
    # Error in function (type, msg, asError = TRUE)  : 
    #error:14077102:SSL routines:SSL23_GET_SERVER_HELLO:unsupported protocol
    # which leads to the "simpleWarning" error.
    #
    # The error does not appear if we use base::url() instead of
    # RCurl::getUrl().
    if (doesRCurlRequestUrlExist(request, useragent=useragent)) {
        res <- getRCurlRequestResult(request, useragent=useragent,
            ssl.verifypeer=ssl.verifypeer)
    } else {
        logTrace('Using base::url() for sending request.')
        sUrl <- request$getUrl()$toString()
        logDebug(paste('URL "%s" does not exist according to RCurl.',
            'That may happen with some protocol misunderstanding.',
            'Trying with base::url().'), sUrl)
        res <- getBaseUrlRequestResult(request)
    }

    return(res)
}

#' Build an RCurl::CURLOptions object.
#'
#' @param useragent The user agent identification.
#' @param httpheader The HTTP header to send.
#' @param postfields POST fields, in case of a POST method.
#' @param timeout.ms The timeout in milliseconds.
#' @param verbose Set to TRUE to get verbose output in RCurl.
#' @return An RCurl::CURLOptions object.
makeRCurlOptions <- function(useragent=NULL, httpheader=NULL, postfields=NULL,
    timeout.ms=60000, verbose=FALSE) {
    chk::chk_null_or(useragent, vld=chk::vld_string)
    chk::chk_null_or(httpheader, vld=chk::vld_character)
    chk::chk_null_or(postfields, vld=chk::vld_character)
    chk::chk_whole_number(timeout.ms)
    chk::chk_flag(verbose)

    opts <- list()

    if ( ! is.null(httpheader) && length(httpheader) > 0)
        opts$httpheader <- httpheader

    if ( ! is.null(postfields) && length(postfields) > 0)
        opts$postfields <- postfields

    opts <- RCurl::curlOptions(useragent=useragent, timeout.ms=timeout.ms,
        verbose=verbose, .opts=opts)

    return(opts)
}

#' Test if a URL is valid according to RCurl
#'
#' @param request A BiodbRequest object, from which the URL will be gotten.
#' @param useragent The user agent identification.
#' @return Returns TRUE if the URL
doesRCurlRequestUrlExist <- function(request, useragent=NULL) {
    chk::chk_is(request, 'BiodbRequest')
    chk::chk_null_or(useragent, vld=chk::vld_string)

    opts <- request$getCurlOptions(useragent=useragent)
    sUrl <- request$getUrl()$toString()
    exists <- RCurl::url.exists(sUrl, .opts=opts)
    if ( ! exists)
        logTrace('According to RCurl, URL %s does not exist.', sUrl)

    return(exists)
}

#' Get URL content using RCurl::getURL().
#'
#' @param u The URL as a character value.
#' @param opts A valid RCurl options object.
#' @param enc The encoding.
#' @param header.fct An RCurl header gatherer function.
#' @param ssl.verifypeer Set to TRUE to enable SSL verify peer.
#' @param method The HTTP method to use, either 'get' or 'post'.
#' @param binary Set to TRUE if the content to be retrieved is binary.
#' @return The URL content as a character single value.
getRCurlContent <- function(u, opts=NULL, enc=integer(), header.fct=NULL,
    ssl.verifypeer=TRUE, method=c('get', 'post'), binary=FALSE) {
    chk::chk_string(u)
    chk::chk_null_or(opts, vld=chk::vld_is, class='CURLOptions')
    # TODO Test "enc" param.
    chk::chk_null_or(header.fct, vld=chk::vld_is, class='function')
    chk::chk_flag(ssl.verifypeer)
    chk::chk_flag(binary)
    method <- match.arg(method)
    if (is.null(opts))
        opts <- list()

    # GET                            
    if (method == 'get') {
        if (binary)
            content <- RCurl::getBinaryURL(u, .opts=opts, .encoding=enc,
                ssl.verifypeer=ssl.verifypeer, headerfunction=header.fct)
        else
            content <- RCurl::getURL(u, .opts=opts, .encoding=enc,
                ssl.verifypeer=ssl.verifypeer, headerfunction=header.fct)
        
    # POST
    } else {
        content <- RCurl::postForm(u, .opts=opts, .encoding=enc,
            headerfunction=header.fct)
    }

    return(content)
}

#' Get URL request result using RCurcl::getURL().
#'
#' @param request A BiodbRequest object.
#' @param useragent The user agent identification.
#' @param ssl.verifypeer Set to TRUE to enable SSL verify peer.
#' @return A RequestResult object.
getRCurlRequestResult <- function(request, useragent=NULL,
    ssl.verifypeer=TRUE) {
    chk::chk_is(request, 'BiodbRequest')
    chk::chk_null_or(useragent, vld=chk::vld_string)
    chk::chk_flag(ssl.verifypeer)
    content <- NA_character_
    err_msg <- NULL
    retry <- FALSE
    sUrl <- request$getUrl()$toString()
    logTrace('Using RCurl package for sending request (%s).', sUrl)

    # Build options
    opts <- request$getCurlOptions(useragent=useragent)

    # Create HTTP header object (to receive HTTP information from server).
    header <- RCurl::basicHeaderGatherer()

    curl.error <- NULL
    header$reset()
    enc <- request$getEncoding()
    content <- tryCatch(expr={ getRCurlContent(sUrl, opts=opts, enc=enc,
        header.fct=header$update, ssl.verifypeer=ssl.verifypeer,
        method=request$getMethod()) },
        PEER_FAILED_VERIFICATION=function(err) { retry=TRUE ; curl.error=err ;
            NULL },
        GenericCurlError=function(err) { retry=TRUE ; curl.error=err ; NULL },
        error=function(err) { retry=FALSE ; curl.error=err ; NULL })

    # RCurl error
    if ( ! is.null(curl.error))
        err_msg <- paste0("RCurl error: ", curl.error)

    # Get header information sent by server
    hdr <- NULL
    if (is.null(err_msg)) {
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

    status <- if (is.null(hdr) || ! 'status' %in% names(hdr)) 0 else hdr$status
    statusMessage <- if (is.null(hdr) || ! 'statusMessage' %in% names(hdr))
        '' else hdr$statusMessage
    retryAfter <- if(is.null(hdr) || ! 'Retry-After' %in% names(hdr)) NULL else
        hdr[['Retry-After']]
    location <- if(is.null(hdr) || ! 'location' %in% names(hdr)) NULL else
        hdr[['location']]

    return(RequestResult$new(content=content, retry=retry, errMsg=err_msg,
        status=status, statusMessage=statusMessage, retryAfter=retryAfter,
        location=location))
}

#' Get URL content using base::url().
#'
#' @param u The URL as a character value.
#' @param binary Set to TRUE if the content to be retrieved is binary.
#' @return The URL content as a character single value.
getBaseUrlContent <- function(u, binary=FALSE) {
    chk::chk_string(u)

    # Open URL and get URL descriptor
    ud <- base::url(u)

    # Get content
    content <- tryCatch(expr=paste(readLines(ud, warn=FALSE), collapse="\n"),
        warning=function(w) NULL,
        error=function(e) NULL)

    # Close URL descriptor
    close(ud)

    # Convert to raw
    if (binary)
        content <- charToRaw(content)

    return(content)
}

#' Get URL request result using base::url().
#'
#' @param request A BiodbRequest object.
#' @return A RequestResult object.
getBaseUrlRequestResult <- function(request) {
    chk::chk_is(request, 'BiodbRequest')
    sUrl <- request$getUrl()$toString()
    logTrace('Using base::url() for sending request (%s).', sUrl)

    if (request$getMethod() != 'get')
        error('Request method "%s" is not hanlded by base::url().',
            request$getMethod())

    content <- getBaseUrlContent(sUrl)

    if (! is.null(content) && ( ! is.character(content) || content == ''))
        content <- NULL

    err <- if (is.null(content)) 'Error when retrieving URL content' else
        NULL

    status <- if (is.null(content)) .HTTP.STATUS.NOT.FOUND else
        .HTTP.STATUS.OK

    res <- RequestResult$new(content=content, retry=FALSE, errMsg=err,
        status=status)

    return(res)
}
