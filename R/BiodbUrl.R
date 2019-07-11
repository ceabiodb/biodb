# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbUrl {{{1
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

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

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

# Get domain {{{3
################################################################################

getDomain=function() {

    domain <- sub('^.+://([^/]+)(/.*)?$', '\\1', .self$.url[[1]], perl=TRUE)

    return(domain)
},

# Set URL {{{3
################################################################################

setUrl=function(url) {
    .self$.url <- url
},

# Set parameter {{{3
################################################################################

setParam=function(key, value) {
    .self$.params[[key]] <- value
},

# Show {{{3
################################################################################

show=function() {
    cat(.self$toString(), "\n", sep='')
},

# To string {{{3
################################################################################

toString=function(encode=TRUE) {

    # Remove '/' at start and end of each element of the URL
    url <- gsub('^/*([^/].*[^/])/*$', '\\1', .self$.url)
    
    # Concatenate URL elements together
    url <- paste(url, collapse='/')

    # Add parameters to URL
    if (length(.self$.params) > 0) {
        
        pn <- names(.self$.params)
        pv <- unname(.self$.params)
        
        # Encode parameter values
        if (encode)
            pv <- vapply(pv, utils::URLencode, FUN.VALUE='', USE.NAMES=FALSE)

        # Build parameters string
        fct <- function(i) {
            if (is.null(pn) || nchar(pn[[i]]) == 0)
                pv[[i]]
            else
                paste(pn[[i]], pv[[i]], sep='=')
        }
        kv.list <- vapply(seq_along(pv), fct, FUN.VALUE='')
        params.str <- paste(kv.list, collapse='&')

        # Concatenate URL with parameters
        url <- paste(url, params.str, sep='?')
    }

    return(url)
}

))
