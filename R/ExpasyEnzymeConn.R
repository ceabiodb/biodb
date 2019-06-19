# vi: fdm=marker ts=4 et cc=80 tw=80

# Class declaration {{{1
################################################################################

#' The connector class to Expasy Enzyme database.
#'
#' This is a concrete connector class. It must never be instantiated directly, but instead be instantiated through the factory \code{\link{BiodbFactory}}. Only specific methods are described here. See super classes for the description of inherited methods.
#'
#' @param name      The name to search for.
#' @param comment   The comment to search for.
#'
#' @include BiodbRemotedbConn.R
#' @export ExpasyEnzymeConn
#' @exportClass ExpasyEnzymeConn
ExpasyEnzymeConn <- methods::setRefClass("ExpasyEnzymeConn", contains=c("BiodbRemotedbConn"))

# Web service enzyme-byname {{{1
################################################################################

ExpasyEnzymeConn$methods( wsEnzymeByName=function(name, retfmt=c('plain', 'request', 'parsed', 'ids')) {
    "Calls enzyme-byname web service and returns the HTML result. See http://enzyme.expasy.org/enzyme-byname.html."

    retfmt <- match.arg(retfmt)

    # Build request
    request <- BiodbRequest(method='get', url=BiodbUrl(url=c(.self$getPropValSlot('urls', 'base.url'), "enzyme-byname.html"), params=name))
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse HTML
    results <- .self$.parseWsReturnedHtml(results=results, retfmt=retfmt)

    return(results)
})

# Web service enzyme-bycomment {{{1
################################################################################

ExpasyEnzymeConn$methods( wsEnzymeByComment=function(comment, retfmt=c('plain', 'request', 'parsed', 'ids')) {
    "Calls enzyme-bycomment web service and returns the HTML result. See http://enzyme.expasy.org/enzyme-bycomment.html."

    retfmt <- match.arg(retfmt)

    # Build request
    request <- BiodbRequest(method='get', url=BiodbUrl(url=c(.self$getPropValSlot('urls', 'base.url'), "enzyme-bycomment.html"), params=comment))
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse HTML
    results <- .self$.parseWsReturnedHtml(results=results, retfmt=retfmt)

    return(results)
})

# Get entry page url {{{1
################################################################################

ExpasyEnzymeConn$methods( getEntryPageUrl=function(id) {

    urls <- rep(NA_character_, length(id))

    # Loop on all IDs
    i <- 0
    for (x in id) {

        i <- i + 1

        # Get four fields of ID
        fields <- strsplit(x, '\\.')[[1]]
        if (length(fields) == 4)
            urls[[i]] <- BiodbUrl(url=c(.self$getPropValSlot('urls', 'base.url'), 'cgi-bin', 'enzyme', 'enzyme-search-ec'), params=list(field1=fields[[1]], field2=fields[[2]], field3=fields[[3]], field4=fields[[4]]))$toString()
    }

    return(urls)
})

# Get entry image url {{{1
################################################################################

ExpasyEnzymeConn$methods( getEntryImageUrl=function(id) {
    return(rep(NA_character_, length(id)))
})

# Private methods {{{1
################################################################################

# Do get entry content request {{{2
################################################################################

ExpasyEnzymeConn$methods( .doGetEntryContentRequest=function(id, concatenate=TRUE) {

    urls <- vapply(id, function(x) BiodbUrl(url=c(.self$getPropValSlot('urls', 'base.url'), 'EC', paste(x, 'txt', sep='.')))$toString(), FUN.VALUE='')

    return(urls)
})

# Parse HTML returned by web services {{{2
################################################################################

ExpasyEnzymeConn$methods( .parseWsReturnedHtml=function(results, retfmt) {

    if (retfmt %in% c('parsed', 'ids')) {

        # Parse HTML
        results <-  XML::htmlTreeParse(results, asText=TRUE, useInternalNodes=TRUE)

        # Get ids
        if (retfmt == 'ids')
            results <- XML::xpathSApply(results, "//a[starts-with(@href,'/EC/')]", XML::xmlValue)
    }

    return(results)
})

# Get entry ids {{{2
################################################################################

ExpasyEnzymeConn$methods( .doGetEntryIds=function(max.results=NA_integer_) {

    # Send request
    ids <- .self$wsEnzymeByComment('e', retfmt='ids')

    return(ids)
})

