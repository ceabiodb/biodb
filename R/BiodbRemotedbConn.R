# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbRemotedbConn {{{1
################################################################################

#' The mother class of all remote database connectors.
#'
#' This is the super class of remote database connectors. It thus defines
#' methods related to remote connection, like the definition of a token, and URL
#' definitions. As with \code{\link{BiodbConn}} class, you won't need to use the
#' constructor. Nevertheless we provide in the Fields section information about
#' the constructor parameters, for eventual developers.
#'
#' @param concatenate   If set to \code{TRUE}, then try to build as few URLs as
#' possible, sending requests with several identifiers at once.
#' @param entry.id      The identifiers (e.g.: accession numbers) as a
#' \code{character vector} of the database entries.
#' @param max.length    The maximum length of the URLs to return, in number of
#' characters.
#'
#' @seealso \code{\link{BiodbConn}}, \code{\link{BiodbRequestScheduler}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get connector
#' conn <- mybiodb$getFactory()$createConn('chemspider')
#'
#' # Get the picture URL of an entry
#' picture.url <- conn$getEntryImageUrl('2')
#'
#' # Get the page URL of an entry
#' page.url <- conn$getEntryPageUrl('2')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @include BiodbConn.R
#' @include BiodbRequestScheduler.R
#' @export BiodbRemotedbConn
#' @exportClass BiodbRemotedbConn
BiodbRemotedbConn <- methods::setRefClass("BiodbRemotedbConn",
    contains="BiodbConn",

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)
    .self$.abstractClass('BiodbRemotedbConn')

    # Register with request scheduler
    .self$getBiodb()$getRequestScheduler()$.registerConnector(.self)
},

# Get entry content from database {{{3
################################################################################

getEntryContentFromDb=function(entry.id) {
    # Default implementation
    return(.self$.doGetEntryContentOneByOne(entry.id))
},

# Get entry content request {{{3
################################################################################

getEntryContentRequest=function(entry.id, concatenate=TRUE, max.length=0) {
    "Get the URL to use in order to get the contents of the specified entries."

    # Copy code from get.entry.url
    # 

    urls <- character(0)

    if (length(entry.id) > 0) {

        # Get full URL
        full.url <- .self$.doGetEntryContentRequest(entry.id,
                                                    concatenate=concatenate)

        # No single URL for multiple IDs
        if ((length(entry.id) > 1 && length(full.url) > 1) || max.length == 0
            || nchar(full.url) <= max.length)
            urls <- full.url

        # full.url is too big, we must split it
        else {
            .self$message('debug', "Split full URL.")

            start <- 1

            # Loop as long as there are IDs
            while (start <= length(entry.id)) {
                # Find max size URL
                a <- start
                b <- length(entry.id)
                while (a < b) {
                    m <- as.integer((a + b) / 2)
                    url <- .self$.doGetEntryContentRequest(entry.id[start:m])
                    if (all(nchar(url) <= max.length) && m != a)
                        a <- m
                    else
                        b <- m
                }
                urls <- c(urls,
                          .self$.doGetEntryContentRequest(entry.id[start:a]))
                start <- a + 1
            }
        }
    }

    return(urls)
},

# Get entry image url {{{3
################################################################################

getEntryImageUrl=function(entry.id) {
    "Get the URL to a picture of the entry (e.g.: a picture of the molecule in
    case of a compound entry)."

    .self$.abstractMethod()
},

# Get entry page url {{{3
################################################################################

getEntryPageUrl=function(entry.id) {
    "Get the URL to the page of the entry on the database web site."

    .self$.abstractMethod()
},

# Private methods {{{2
################################################################################

# Set request scheduler rules {{{3
################################################################################

.setRequestSchedulerRules=function() {
},

# Do get entry content request {{{3
################################################################################

.doGetEntryContentRequest=function(id, concatenate=TRUE) {
    .self$.abstractMethod()
},

# Do get entry content one by one {{{3
################################################################################

.doGetEntryContentOneByOne=function(entry.id) {

    # Initialize return values
    content <- rep(NA_character_, length(entry.id))

    # Get requests
    requests <- .self$getEntryContentRequest(entry.id, concatenate=FALSE)
    
    # Get encoding
    encoding <- .self$getPropertyValue('entry.content.encoding')

    # If requests is a vector of characters, then the method is using the old
    # scheme.
    # We now convert the requests to the new scheme, using class BiodbRequest.
    if (is.character(requests)) {
        fct <- function(x) BiodbRequest(method='get', url=BiodbUrl(x),
                                        encoding=encoding)
        requests <- lapply(requests, fct)
    }

    # Send requests
    scheduler <- .self$getBiodb()$getRequestScheduler()
    for (i in seq_along(requests)) {
        .self$progressMsg(msg='Getting entry contents.', index=i,
                          total=length(requests), first=(i == 1))
        content[[i]] <- scheduler$sendRequest(requests[[i]])
    }

    return(content)
},

# Terminate {{{3
################################################################################

.terminate=function() {

    # Unregister from the request scheduler
    .self$getBiodb()$getRequestScheduler()$.unregisterConnector(.self)
}

))
