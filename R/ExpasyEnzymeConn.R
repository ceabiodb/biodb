# vi: fdm=marker ts=4 et cc=80 tw=80

# ExpasyEnzymeConn {{{1
################################################################################

# Declaration {{{2
################################################################################

#' The connector class to Expasy Enzyme database.
#'
#' This is a concrete connector class. It must never be instantiated directly,
#' but instead be instantiated through the factory \code{\link{BiodbFactory}}.
#' Only specific methods are described here. See super classes for the
#' description of inherited methods.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('expasy.enzyme')
#'
#' # Get an entry
#' e <- conn$getEntry('1.1.1.1')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbRemotedbConn.R
#' @export ExpasyEnzymeConn
#' @exportClass ExpasyEnzymeConn
ExpasyEnzymeConn <- methods::setRefClass("ExpasyEnzymeConn",
    contains="BiodbRemotedbConn",

# Public methods {{{2
################################################################################

methods=list(

# Web service enzyme-byname {{{3
################################################################################

wsEnzymeByName=function(name, retfmt=c('plain', 'request', 'parsed', 'ids')) {
    ":\n\nCalls enzyme-byname web service and returns the HTML result. See
    http://enzyme.expasy.org/enzyme-byname.html.
    \nname: The name to search for.
    \nretfmt: The format to use for the returned value. 'plain' will return the
    raw result from the server, as a character value. 'request' will return a
    BiodbRequest instance containing the request as it would have been sent.
    'parsed' will return an XML object, containing the parsed result. 'ids' will
    return a character vector containing the IDs of the matching entries.
    \nReturned value: Depending on `retfmt`.
    "

    retfmt <- match.arg(retfmt)

    # Build request
    burl <- BiodbUrl(url=c(.self$getPropValSlot('urls', 'base.url'),
                           "enzyme-byname.html"), params=name)
    request <- BiodbRequest(method='get', url=burl)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse HTML
    results <- .self$.parseWsReturnedHtml(results=results, retfmt=retfmt)

    return(results)
},

# Web service enzyme-bycomment {{{3
################################################################################

wsEnzymeByComment=function(comment, retfmt=c('plain', 'request', 'parsed',
                                             'ids')) {
    ":\n\nCalls enzyme-bycomment web service and returns the HTML result. See
    http://enzyme.expasy.org/enzyme-bycomment.html.
    \ncomment: The comment to search for.
    \nretfmt: The format to use for the returned value. 'plain' will return the
    raw result from the server, as a character value. 'request' will return a
    BiodbRequest instance containing the request as it would have been sent.
    'parsed' will return an XML object, containing the parsed result. 'ids' will
    return a character vector containing the IDs of the matching entries.
    \nReturned value: Depending on `retfmt`.
    "

    retfmt <- match.arg(retfmt)

    # Build request
    burl <- BiodbUrl(url=c(.self$getPropValSlot('urls', 'base.url'),
                           "enzyme-bycomment.html"), params=comment)
    request <- BiodbRequest(method='get', url=burl)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse HTML
    results <- .self$.parseWsReturnedHtml(results=results, retfmt=retfmt)

    return(results)
},

# Get entry page URL {{{3
################################################################################

getEntryPageUrl=function(id) {
    # Overrides super class' method.

    urls <- rep(NA_character_, length(id))

    # Loop on all IDs
    i <- 0
    for (x in id) {

        i <- i + 1

        # Get four fields of ID
        fields <- strsplit(x, '\\.')[[1]]
        if (length(fields) == 4) {
            u <- c(.self$getPropValSlot('urls', 'base.url'), 'cgi-bin',
                   'enzyme', 'enzyme-search-ec')
            p <- list(field1=fields[[1]], field2=fields[[2]],
                      field3=fields[[3]], field4=fields[[4]])
            urls[[i]] <- BiodbUrl(url=u, params=p)$toString()
        }
    }

    return(urls)
},

# Private methods {{{2
################################################################################

# Do get entry content request {{{3
################################################################################

.doGetEntryContentRequest=function(id, concatenate=TRUE) {

    fct <- function(x) {
        u <- c(.self$getPropValSlot('urls', 'base.url'), 'EC',
               paste(x, 'txt', sep='.'))
        return(BiodbUrl(url=u)$toString())
    }
    urls <- vapply(id, fct, FUN.VALUE='')

    return(urls)
},

# Parse HTML returned by web services {{{3
################################################################################

.parseWsReturnedHtml=function(results, retfmt) {

    if (retfmt %in% c('parsed', 'ids')) {

        # Parse HTML
        results <-  XML::htmlTreeParse(results, asText=TRUE,
                                       useInternalNodes=TRUE)

        # Get ids
        if (retfmt == 'ids')
            results <- XML::xpathSApply(results,
                                        "//a[starts-with(@href,'/EC/')]",
                                        XML::xmlValue)
    }

    return(results)
},

# Get entry ids {{{3
################################################################################

.doGetEntryIds=function(max.results=NA_integer_) {

    # Send request
    ids <- .self$wsEnzymeByComment('e', retfmt='ids')

    return(ids)
}

))
