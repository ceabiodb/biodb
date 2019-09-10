# vi: fdm=marker ts=4 et cc=80 tw=80

# KeggConn {{{1
################################################################################

#' The connector abstract class to KEGG databases.
#'
#' This is the mother class of all KEGG connectors. It defines code common to
#' all KEGG connectors.
#'
#' The constructor accepts the following arguments:
#'
#' db.name: The database name as defined in
#' http://www.kegg.jp/kegg/docs/keggapi.html.
#'
#' db.abbrev: The database abbreviated name, as defined in
#' http://www.kegg.jp/kegg/docs/keggapi.html.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{BiodbRemotedbConn}}
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector to a KEGG database
#' conn <- mybiodb$getFactory()$createConn('kegg.compound')
#' 
#' # Search for an entry
#' conn$wsFind('NADPH', retfmt='parsed')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbRemotedbConn.R
#' @export KeggConn
#' @exportClass KeggConn
KeggConn <- methods::setRefClass("KeggConn",
    contains="BiodbRemotedbConn",
    fields=list(
        .db.name="character",
        .db.abbrev="character"
    ),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(db.name=NA_character_, db.abbrev=NA_character_, ...) {

    callSuper(...)
    .self$.abstractClass('KeggConn')

    # Set name
    if (is.null(db.name) || is.na(db.name))
        .self$message('error', "You must set a name for this KEGG database.")
    .self$.db.name <- db.name

    # Set abbreviation
    .self$.db.abbrev <- db.abbrev
},

# Get entry page url {{{3
################################################################################

getEntryPageUrl=function(id) {
    # Overrides super class' method.
    
    u <- c(.self$getPropValSlot('urls', 'entry.page.url'), 'www_bget')
    p <- .self$.completeEntryId(id)
    fct <- function(x) BiodbUrl(url=u, params=p)$toString()

    return(vapply(id, fct, FUN.VALUE=''))
},

# Web service list {{{3
################################################################################

wsList=function(retfmt=c('plain', 'request', 'ids')) {
    ":\n\nGets the full list of entry IDs. See http://www.kegg.jp/kegg/docs/keggapi.html for
    details.
    \nretfmt: Use to set the format of the returned value. 'plain' will return
    the raw result from the server, as a character value. 'request' will return
    the request as it would have been sent, as a BiodbRequest object. 'ids' will
    return a character vector containing entry IDs.
    \nReturned value: Depending on `retfmt`.
    "

    # Not implemented for genes database
    if (.self$.db.name == 'genes')
        return(character())

    retfmt <- match.arg(retfmt)

    # Build request
    u <- c(.self$getPropValSlot('urls', 'ws.url'), 'list', .self$.db.name)
    url <- BiodbUrl(url=u)
    request <- BiodbRequest(url=url)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Extract IDs
    if (retfmt == 'ids') {
        results <- strsplit(results, "\n")[[1]]

        if ( ! is.na(.self$.db.abbrev) && nchar(.self$.db.abbrev) > 0)
            results <- sub('^[^:]+:([^\\s]+)\\s.*$', '\\1', results, perl=TRUE)
        else
            results <- sub('^([^\\s]+)\\s.*$', '\\1', results, perl=TRUE)
    }

    return(results)
},

# Web service find {{{3
################################################################################

wsFind=function(query, retfmt=c('plain', 'request', 'parsed', 'ids')) {
    ":\n\nSearches for entries. See http://www.kegg.jp/kegg/docs/keggapi.html for
    details.
    \nquery: The query to send to the database web service.
    \nretfmt: Use to set the format of the returned value. 'plain' will return
    the raw result from the server, as a character value. 'request' will return
    the request as it would have been sent, as a BiodbRequest object. 'parsed'
    will return a data frame. 'ids' will return a character vector containing
    the IDs of the matching entries.
    \nReturned value: Depending on `retfmt`.
    "

    retfmt <- match.arg(retfmt)

    # Build request
    u <- c(.self$getPropValSlot('urls', 'ws.url'), 'find', .self$.db.name,
           query)
    url <- BiodbUrl(url=u)
    request <- BiodbRequest(url=url)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse
    if (retfmt != 'plain') {

        # Parse data frame
        readtc <- textConnection(results, "r", local=TRUE)
        df <- read.table(readtc, sep="\t", quote='', stringsAsFactors=FALSE)
        close(readtc)
        results <- df

        if (retfmt == 'ids')
            results <- results[[1]]
    }

    return(results)
},

# Search by name {{{3
################################################################################

searchByName=function(name, max.results=NA_integer_) {
    # Overrides super class' method.

    ids <- NULL

    # Search by name
    if ( ! is.null(name) && ! is.na(name)) {
        ids <- .self$wsFind(name, retfmt='ids')
        if ( ! is.na(.self$.db.abbrev) && nchar(.self$.db.abbrev) > 0)
            ids <- sub('^[^:]*:', '', ids)
    }

    # Cut
    if ( ! is.na(max.results) && max.results > 0 && max.results < length(ids))
        ids <- ids[seq_len(max.results)]

    return(ids)
},

# Private methods {{{2
################################################################################

# Complete entry id {{{3
################################################################################

.completeEntryId=function(id) {

    if ( ! is.na(.self$.db.abbrev) && nchar(.self$.db.abbrev) > 0)
        id <- paste(.self$.db.abbrev, id, sep=':')

    return(id)
},

# Get entry content request {{{3
################################################################################

.doGetEntryContentRequest=function(id, concatenate=TRUE) {
    
    fct <- function(x) {
        u <- c(.self$getPropValSlot('urls', 'ws.url'), 'get', x)
        BiodbUrl(url=u)$toString()
    }
    
    return(vapply(id, fct, FUN.VALUE=''))
},

# Get entry ids {{{3
################################################################################

.doGetEntryIds=function(max.results=NA_integer_) {

    # Get IDs
    ids <- .self$wsList(retfmt='ids')

    return(ids)
}

))
