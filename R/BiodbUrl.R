#' Class URL.
#'
#' This class represents a URL object that can be used in requests.
#'
#' The following arguments are accepted by the constructor:
#'
#' url: The URL to access, as a character string.
#'
#' params: The list of parameters to use with the URL.
#'
#' @seealso \code{\link{BiodbRequestScheduler}}, \code{\link{BiodbRequest}}.
#'
#' @examples
#' # Create a URL object
#' u <- c("https://www.uniprot.org", "uniprot")
#' p <- c(query="reviewed:yes+AND+organism:9606",
#'        columns='id,entry name,protein names',
#'        format="tab")
#' url <- BiodbUrl(url=u, params=p)
#' url$toString()
#'
#' @import methods
#' @export BiodbUrl
#' @exportClass BiodbUrl
BiodbUrl <- methods::setRefClass("BiodbUrl",
    fields=list(
        .url='character',
        .params='character'
        ),

methods=list(

initialize=function(url=character(), params=character()) {

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
},

getDomain=function() {
    ":\n\nGets the domain.
    \nReturned value: None.
    "

    domain <- sub('^.+://([^/]+)(/.*)?$', '\\1', .self$.url[[1]], perl=TRUE)

    return(domain)
},

setUrl=function(url) {
    ":\n\nSets the base URL string.
    \nurl: The base URL string.
    \nReturned value: None.
    "

    .self$.url <- url
},

setParam=function(key, value) {
    ":\n\nSets a parameter.
    \nkey:
    \nvalue:
    \nReturned value: None.
    "

    .self$.params[[key]] <- value
},

show=function() {
    ":\n\nDisplays information about this instance.
    \nReturned value: None.
    "

    cat(.self$toString(), "\n", sep='')
},

toString=function(encode=TRUE) {
    ":\n\nGets the URL as a string representation.
    \nencode: If set to TRUE, then encodes the URL.
    \nReturned value: The URL as a string, with all parameters and values set.
    "

    # Remove '/' at start and end of each element of the URL
    url <- gsub('^/*([^/].*[^/])/*$', '\\1', .self$.url)

    # Concatenate URL elements together
    url <- paste(url, collapse='/')

    # Add parameters to URL
    if (length(.self$.params) > 0) {

        pn <- names(.self$.params)
        pv <- unname(.self$.params)

        # Build parameters string
        fct <- function(i) {
            if (is.null(pn) || nchar(pn[[i]]) == 0)
                pv[[i]]
            else
                paste(pn[[i]], pv[[i]], sep='=')
        }
        kv.list <- vapply(seq_along(pv), fct, FUN.VALUE='')
        params.str <- paste(kv.list, collapse='&')

        # Encode parameter values
        if (encode)
            params.str <- utils::URLencode(params.str)

        # Concatenate URL with parameters
        url <- paste(url, params.str, sep='?')
    }

    return(url)
}

))
