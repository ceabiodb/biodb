#' Get URL content using RCurl::getURL().
#'
#' @param u The URL as a character value.
#' @param useragent The user agent identification.
#' @return The URL content as a character single value.
getRCurlContent <- function(u, useragent=NULL) {
    chk::chk_string(u)
    chk::chk_null_or(u, chk::chk_string)
}

#' Get URL request result using RCurcl::getURL().
#'
#' @param request A BiodbRequest object.
#' @param opts A valid RCurl options object.
#' @param ssl.verifypeer Set to TRUE to enable SSL verify peer.
#' @return A RequestResult object.
getRCurlRequestResult <- function(request, opts, ssl.verifypeer=TRUE) {
    chk::chk_is(request, 'BiodbRequest')
    chk::chk_is(opts, 'CURLOptions')
    chk::chk_flag(ssl.verifypeer)
    content <- NA_character_
    err_msg <- NULL
    retry <- FALSE

    # Create HTTP header object (to receive HTTP information from server).
    header <- RCurl::basicHeaderGatherer()

    curl.error <- NULL
    header$reset()
    sUrl <- request$getUrl()$toString()
    enc <- request$getEncoding()
    content <- tryCatch(expr={
                            
        # GET                            
        if (request$getMethod() == 'get')
            RCurl::getURL(sUrl, .opts=opts, ssl.verifypeer=ssl.verifypeer,
                .encoding=enc, headerfunction=header$update)
            
        # POST
        else
            RCurl::postForm(sUrl, .opts=opts, .encoding=enc,
                headerfunction=header$update)
        },
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
#' @return The URL content as a character single value.
getBaseUrlContent <- function(u) {
    chk::chk_string(u)

    # Open URL and get URL descriptor
    ud <- url(u)

    # Get content
    content <- tryCatch(expr=paste(readLines(ud), collapse="\n"),
        warning=function(w) NULL,
        error=function(e) NULL)

    # Close URL descriptor
    close(ud)

    return(content)
}

#' Get URL request result using base::url().
#'
#' @param request A BiodbRequest object.
#' @return A RequestResult object.
getBaseUrlRequestResult <- function(request) {
    chk::chk_is(request, 'BiodbRequest')

    if (request$getMethod() != 'get')
        error('Request method "%s" is not hanlded by base::url().',
            request$getMethod())

    content <- getBaseUrlContent(request$getUrl()$toString())

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
