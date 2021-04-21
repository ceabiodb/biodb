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
#' url <- BiodbUrl$new(url=u, params=p)
#' url$toString()
#'
#' @import R6
#' @export
BiodbUrl <- R6::R6Class("BiodbUrl",

public=list(

initialize=function(url=character(), params=character()) {

    # Set URL
    private$url <- url

    # Set parameters
    if (is.list(params))
        params <- unlist(params)
    if ( ! is.character(params)) {
        names <- names(params)
        params <- as.character(params)
        names(params) <- names
    }
    private$params <- params
},

#' @description
#' Gets the domain.
#' @return None.
getDomain=function() {

    domain <- sub('^.+://([^/]+)(/.*)?$', '\\1', private$url[[1]], perl=TRUE)

    return(domain)
},

#' @description
#' Sets the base URL string.
#' @param url The base URL string.
#' @return None.
setUrl=function(url) {

    private$url <- url
},

#' @description
#' Sets a parameter.
#' @param key 
#' @param value 
#' @return None.
setParam=function(key, value) {

    private$params[[key]] <- value
},

#' @description
#' Displays information about this instance.
#' @return None.
print=function() {

    cat(self$toString(), "\n", sep='')
 
    return(invisible(self))
},

#' @description
#' Gets the URL as a string representation.
#' @param encode If set to TRUE, then encodes the URL.
#' @return The URL as a string, with all parameters and values set.
toString=function(encode=TRUE) {

    # Remove '/' at start and end of each element of the URL
    u <- gsub('^/*([^/].*[^/])/*$', '\\1', private$url)

    # Concatenate URL elements together
    u <- paste(u, collapse='/')

    # Add parameters to URL
    if (length(private$params) > 0) {

        pn <- names(private$params)
        pv <- unname(private$params)

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
        u <- paste(u, params.str, sep='?')
    }

    # Encode parameter values
    if (encode)
        u <- utils::URLencode(u)

    return(u)
}
),

private=list(
    url=NULL,
    params=NULL
))
