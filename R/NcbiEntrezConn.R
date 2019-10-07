# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiEntrezConn {{{1
################################################################################

# Declaration {{{2
################################################################################

#' NCBI Entrez connector abstract class.
#'
#' This is an abstract class, mother class of all NCBI Entrez connector classes.
#'
#' This is the connector class for a NCBI Gene database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('ncbi.gene')
#'
#' # Get an entry
#' e <- conn$getEntry('2833')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include NcbiConn.R
#' @export NcbiEntrezConn
#' @exportClass NcbiEntrezConn
NcbiEntrezConn <- methods::setRefClass("NcbiEntrezConn",
    contains="NcbiConn",
    fields=list(
        .entrez.name="character",
        .entrez.tag='character',
        .entrez.id.tag='character'),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(entrez.name=NA_character_, entrez.tag=NA_character_,
                    entrez.id.tag=NA_character_, ...) {

    # Call parent constructor
    callSuper(...)
    .self$.abstractClass('NcbiEntrezConn')

    # Set name
    if (is.null(entrez.name) || is.na(entrez.name))
        .self$error("You must set an Entrez name for this NCBI database.")
    .self$.entrez.name <- entrez.name

    # Set tag
    .self$.entrez.tag <- if (is.null(entrez.tag)) NA_character_ else entrez.tag

    # Set ID tag
    .self$.entrez.id.tag <- (if (is.null(entrez.id.tag)) NA_character_
                             else entrez.id.tag)
},

# Web service efetch {{{3
################################################################################

wsEfetch=function(id, rettype=NA_character_, retmode=NA_character_,
                  retfmt=c('plain', 'parsed', 'request')) {
    ":\n\nCalls Entrez efetch web service. See
    https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EFetch.
    \nid: A character vector of entry IDs.
    \nrettype: The retrieval type. See NCBI documentation.
    \nretmode: The retrieval mode. See NCBI documentation.
    \nretfmt: Use to set the format of the returned value. 'plain' will return
    the raw results from the server, as a character value. 'parsed' will return
    the parsed results, as an XML object. 'request' will return a BiodbRequest
    object representing the request as it would have been sent.
    \nReturned value: Depending on `retfmt` parameter.
    "

    retfmt <- match.arg(retfmt)

    # Build request
    params <- c(db=.self$.entrez.name, id=paste(id, collapse=','))
    if ( ! is.na(rettype))
        params <- c(params, rettype=rettype)
    if ( ! is.na(retmode))
        params <- c(params, retmode=retmode)
    u <- c(.self$getPropValSlot('urls', 'ws.url'), 'efetch.fcgi')
    url <- BiodbUrl(url=u, params=params)
    request <- .self$makeRequest(method='get', url=url)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse
    if (retfmt == 'parsed' && retmode == 'xml')
        results <-  XML::xmlInternalTreeParse(results, asText=TRUE)

    return(results)
},

# Web service esearch {{{3
################################################################################

wsEsearch=function(term, field=NA_character_, retmax=NA_integer_,
                   retfmt=c('plain', 'parsed', 'request', 'ids')) {
    ":\n\nCalls Entrez esearch web service. See
    https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch.
    \nterm: Text query. See NCBI documentation.
    \nfield: Entrez field to which to limit the search. See NCBI documentation.
    \nretmax: Maximum number of entry IDs to return.
    \nretfmt: Use to set the format of the returned value. 'plain' will return
    the raw results from the server, as a character value. 'parsed' will return
    the parsed results, as an XML object. 'request' will return a BiodbRequest
    object representing the request as it would have been sent. 'ids' will
    return a character vector containing the IDs of the matching entries.
    \nReturned value: Depending on `retfmt` parameter.
    "

    retfmt <- match.arg(retfmt)

    # Build request
    params <- c(db=.self$.entrez.name, term=term)
    if ( ! is.na(field))
        params <- c(params, field=field)
    if ( ! is.null(retmax) && ! is.na(retmax) && retmax > 0)
        params <- c(params, retmax=as.integer(retmax))
    u <- c(.self$getPropValSlot('urls', 'ws.url'), 'esearch.fcgi')
    url <- BiodbUrl(url=u, params=params)
    request <- .self$makeRequest(method='get', url=url)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse results
    if (retfmt != 'plain') {

        # Parse XML
        results <-  XML::xmlInternalTreeParse(results, asText=TRUE)

        # Get IDs
        if (retfmt == 'ids')
            results <- XML::xpathSApply(results, "//IdList/Id", XML::xmlValue)
    }

    return(results)
},

# Web service einfo {{{3
################################################################################

wsEinfo=function(retfmt=c('plain', 'request', 'parsed')) {
    ":\n\nCalls Entrez einfo web service, returning information about this
    database. See https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EInfo.
    \nretfmt: Use to set the format of the returned value. 'plain' will return
    the raw results from the server, as a character value. 'parsed' will return
    the parsed results, as an XML object. 'request' will return a BiodbRequest
    object representing the request as it would have been sent.
    \nReturned value: Depending on `retfmt` parameter.
    "

    retfmt <- match.arg(retfmt)

    # Build request
    params <- c(db=.self$.entrez.name, version='2.0')
    u <- c(.self$getPropValSlot('urls', 'ws.url'), 'einfo.fcgi')
    url <- BiodbUrl(url=u, params=params)
    request <- .self$makeRequest(method='get', url=url)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse XML
    if (retfmt == 'parsed')
        results <-  XML::xmlInternalTreeParse(results, asText=TRUE)

    return(results)
},

# Get nb entries {{{3
################################################################################

getNbEntries=function(count=FALSE) {
    # Overrides super class' method.

    # Send request
    xml <- .self$wsEinfo(retfmt='parsed')

    # Get number of elements
    n <- XML::xpathSApply(xml, "//Count", XML::xmlValue)
    n <- as.integer(n)

    return(n)
},

# Get entry content from database {{{3
################################################################################

getEntryContentFromDb=function(entry.id) {
    # Overrides super class' method.

    # Debug
    .self$info("Get entry content(s) for ", length(entry.id)," id(s)...")

    URL.MAX.LENGTH <- 2048
    concatenate <- TRUE
    done <- FALSE

    while ( ! done) {

        done <- TRUE

        # Initialize return values
        content <- rep(NA_character_, length(entry.id))

        # Get URL requests
        url.requests <- .self$getEntryContentRequest(entry.id,
                                                     concatenate=concatenate,
                                                     max.length=URL.MAX.LENGTH)

        # Loop on all URLs
        for (url in url.requests) {

            # Send request
            xmlstr <- .self$getBiodb()$getRequestScheduler()$getUrl(url)

            if (is.na(xmlstr) || length(grep('<ERROR>', xmlstr)) > 0) {
                if (concatenate) {
                    .self$caution("Something went wrong while downloading",
                                  " several entries at once.")
                    concatenate <- FALSE
                    done <- FALSE
                    break
                }
                next
            }

            # Parse XML
            xml <-  XML::xmlInternalTreeParse(xmlstr, asText=TRUE)

            # Get returned IDs
            xpath <- paste0("//", .self$.entrez.id.tag)
            returned.ids <- XML::xpathSApply(xml, xpath, XML::xmlValue)

            # Store contents
            nodes <- XML::getNodeSet(xml, paste0("//", .self$.entrez.tag))
            c <- vapply(nodes, XML::saveXML, FUN.VALUE='')
            content[match(returned.ids, entry.id)] <- c
        }
    }

    return(content)
},

# Private methods {{{2
################################################################################

# Do get entry content request {{{3
################################################################################

.doGetEntryContentRequest=function(id, concatenate=TRUE) {

    if (concatenate)
        urls <- .self$wsEfetch(id, retmode='xml',
                               retfmt='request')$getUrl()$toString()
    else {
        fct <- function(single.id) { 
            .self$wsEfetch(single.id, retmode='xml',
                           retfmt='request')$getUrl()$toString()
        }
        urls <- vapply(id, fct, FUN.VALUE='')
    }

    return(urls)
},

# Get entry ids {{{3
################################################################################

.doGetEntryIds=function(max.results=NA_integer_) {

    .self$caution("Method using a last resort solution for its implementation.",
                  " Returns only a small subset of Ncbi entries.")

    retmax <- if (is.na(max.results)) 1000000 else max.results
    return(.self$wsEsearch(term='e', retmax=retmax, retfmt='ids'))
}

))
