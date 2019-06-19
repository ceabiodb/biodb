# vi: fdm=marker ts=4 et cc=80 tw=80

# Class declaration {{{1
################################################################################

#' Class URL.
#'
#' This class represents a URL object that can be used in requests.
#'
#' @param url           The URL to access, as a character string.
#' @param params        The list of parameters to use with the URL.
#'
#' @seealso \code{\link{BiodbRequestScheduler}}, \code{\link{BiodbRequest}}.
#'
#' @import methods
#' @export BiodbUrl
#' @exportClass BiodbUrl
BiodbUrl <- methods::setRefClass("BiodbUrl", fields=list(.url='character', .params='character'))

# Initialize {{{1
################################################################################

BiodbUrl$methods( initialize=function(url=character(), params=character()) {

    # Set URL
    .self$.url <- url

    # Set parameters
    if (is.list(params))
        params <- unlist(params)
    if ( ! is.character(params)) {
        names <- names(params)
        params <- as.character(params)
        names(params) <- names
    }
    .self$.params <- params
})

# Get domain {{{1
################################################################################

BiodbUrl$methods( getDomain=function() {

    domain <- sub('^.+://([^/]+)(/.*)?$', '\\1', .self$.url[[1]], perl=TRUE)

    return(domain)
})

# Set URL {{{1
################################################################################

BiodbUrl$methods( setUrl=function(url) {
    .self$.url <- url
})

# Set parameter {{{1
################################################################################

BiodbUrl$methods( setParam=function(key, value) {
    .self$.params[[key]] <- value
})

# To string {{{1
################################################################################

BiodbUrl$methods( toString=function(encode=TRUE) {

    # Build URL
    url <- gsub('^/*([^/].*[^/])/*$', '\\1', .self$.url) # Remove '/' at start and end of each element
    url <- paste(url, collapse='/') # Concatenate elements together

    # Add parameters to URL
    if (length(.self$.params) > 0) {
        
        pn <- names(.self$.params)
        pv <- unname(.self$.params)
        
        # Encode parameter values
        if (encode)
            pv <- vapply(pv, utils::URLencode, FUN.VALUE='', USE.NAMES=FALSE)

        # Build parameters string
        kv.list <- vapply(seq_along(pv), function(i) if (is.null(pn) || nchar(pn[[i]]) == 0) pv[[i]] else paste(pn[[i]], pv[[i]], sep='='), FUN.VALUE='')
        params.str <- paste(kv.list, collapse='&')

        # Concatenate URL with parameters
        url <- paste(url, params.str, sep='?')
    }

    return(url)
})
