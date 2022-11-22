.HTTP.STATUS.OK <- 200
.HTTP.STATUS.FOUND <- 302
.HTTP.STATUS.NOT.FOUND <- 404
.HTTP.STATUS.REQUEST.TIMEOUT <- 408
.HTTP.STATUS.INTERNAL.SERVER.ERROR <- 500
.HTTP.STATUS.SERVICE.UNAVAILABLE <- 503

#' Class RequestResult.
#'
#' Represents the result of a request.
#'
#' @import chk
#' @import R6
RequestResult <- R6::R6Class("RequestResult",

public=list(
            
#' @description
#' New instance initializer.
#' @param content The result content.
#' @param retry If request should be resent.
#' @param errMsg Error message.
#' @param status HTTP status.
#' @param statusMessage Status message.
#' @param retryAfter Time after which to retry.
#' @param location New location.
#' @return Nothing.
initialize=function(content=NULL, retry=FALSE, errMsg=NULL, status=0,
    statusMessage='', retryAfter=NULL, location=NULL) {
    chk::chk_null_or(content, vld=chk::vld_character) # May be NA
    chk::chk_null_or(content, vld=chk::vld_length, length=1L)
    chk::chk_flag(retry)
    chk::chk_null_or(errMsg, vld=chk::vld_string)
    chk::chk_whole_number(status)
    chk::chk_string(statusMessage)
    chk::chk_null_or(retryAfter, vld=chk::vld_string)
    chk::chk_null_or(location, vld=chk::vld_string)
    
    private$content <- content
    private$retry <- retry
    private$errMsg <- errMsg
    private$status <- status
    private$statusMessage <- statusMessage
    private$retryAfter <- retryAfter
    private$location <- location
}

#' @description
#' Get content.
#' @return The content as a character value or NULL.
,getContent=function() {
    return(private$content)
}

#' @description
#' Get the retry flag.
#' @return TRUE if the URL request should be sent again, FALSE otherwise.
,getRetry=function() {
    return(private$retry)
}

#' @description
#' Get the error message.
#' @return The error message as a character value or NULL.
,getErrMsg=function() {
    return(private$errMsg)
}

#' @description
#' Get the HTTP status of the response.
#' @return The status as an integer.
,getStatus=function() {
    return(private$status)
}

#' @description
#' Get the time to wait before retrying.
#' @return The time.
,getRetryAfter=function() {
    return(private$retryAfter)
}

#' @description
#' Get the redirect location.
#' @return The redirect location as a character value or NULL.
,getLocation=function() {
    return(private$location)
}

#' @description
#' Process possible HTTP error.
#' @return Nothing.
,processRequestErrors=function() {

    # Recoverable HTTP errors
    lst <- c(.HTTP.STATUS.NOT.FOUND, .HTTP.STATUS.REQUEST.TIMEOUT,
        .HTTP.STATUS.INTERNAL.SERVER.ERROR, .HTTP.STATUS.FOUND,
        .HTTP.STATUS.SERVICE.UNAVAILABLE) 
    if (private$status %in% lst) {
        private$addErrMsg(paste0("HTTP error ", private$status," (\"",
            private$statusMessage, "\")."))
        if ( ! is.null(private$retryAfter))
            private$addErrMsg(paste("Retry after", private$retryAfter,
                "."))
        if ( ! is.null(private$location))
            private$addErrMsg(paste("Redirect location to", private$location))
        private$retry <- TRUE
    }

    # Other HTTP errors
    if (is.null(private$errMsg) && private$status != 0 &&
        private$status != .HTTP.STATUS.OK) {
        private$addErrMsg(paste0("Unrecoverable HTTP error ", private$status,
            " (\"", private$statusMessage, "\")."))
        if ( ! is.null(private$retryAfter))
            private$addErrMsg(paste0("Retry after ", private$retryAfter,
                "."))
        private$content <- NA_character_
        private$retry <- FALSE
    }

    # Proxy server error
    # This happens sometime with NCBI CCDS server.
    if ( ! is.null(private$content) && ! is.na(private$content)
        && length(grep('The proxy server could not handle the request',
        unname(private$content))) > 0) {
        logDebug('Found proxy error message in content.')
        private$addErrMsg("Error between the proxy and the main server.")
        private$content <- NA_character_
        private$retry <- FALSE
    }
}

),

private=list(
    content=NULL
    ,retry=NULL
    ,errMsg=NULL
    ,status=NULL
    ,statusMessage=NULL
    ,retryAfter=NULL
    ,location=NULL

,addErrMsg=function(msg) {
    chk::chk_string(msg)
    
    if (is.null(private$errMsg))
        private$errMsg <- msg
    else
        private$errMsg <- paste(private$errMsg, msg)
}
))
