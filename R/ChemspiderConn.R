# vi: fdm=marker ts=4 et cc=80 tw=80

# ChemspiderConn {{{1
################################################################################

# Declaration {{{2
################################################################################

#' The connector class to ChemSpider database.
#'
#' This is a concrete connector class. It must never be instantiated directly,
#' but instead be instantiated through the factory \code{\link{BiodbFactory}}.
#' Only specific methods are described here. See super classes for the
#' description of inherited methods.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{BiodbRemotedbConn}},
#' \code{\link{BiodbCompounddbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('chemspider')
#'
#' # Get an entry
#' e <- conn$getEntry('2')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbCompounddbConn.R
#' @include BiodbRemotedbConn.R
#' @export ChemspiderConn
#' @exportClass ChemspiderConn
ChemspiderConn <- methods::setRefClass("ChemspiderConn",
    contains=c("BiodbRemotedbConn", "BiodbCompounddbConn"),

# Public methods {{{2
################################################################################

methods=list(

# Get entry content from database {{{3
################################################################################

getEntryContentFromDb=function(entry.id) {
    # Overrides super class' method.

    # Initialize return values
    content <- rep(NA_character_, length(entry.id))

    # Get requests
    requests <- .self$getEntryContentRequest(entry.id, concatenate=TRUE)

    # Loop on all requests
    for (request in requests) {

        # Send request
        results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

        if ( ! is.null(results) && ! is.na(results)) {

            # Parse results
            results <- jsonlite::fromJSON(results, simplifyDataFrame=FALSE)

            # Multiple records
            records <- if ('records' %in% names(results)) results[['records']]
                else list(results)

            # Store record contents
            for (record in records)
                if ('id' %in% names(record)) {
                    c <-  jsonlite::toJSON(record, pretty=TRUE,
                                           digits=NA_integer_)
                    content[entry.id == record$id] <- c
                }
        }
    }

    return(content)
},


# Get entry page url {{{3
################################################################################

getEntryPageUrl=function(id) {
    # Overrides super class' method.

    fct <- function(x) {
        url <- c(.self$getPropValSlot('urls', 'base.url'),
                 paste('Chemical-Structure', x, 'html', sep='.'))
        BiodbUrl(url=url)$toString()
    }

    return(vapply(id, fct, FUN.VALUE=''))
},

# Get entry image url {{{3
################################################################################

getEntryImageUrl=function(id) {
    # Overrides super class' method.

    fct <- function(x) {
        url <- c(.self$getPropValSlot('urls', 'base.url'),
                 'ImagesHandler.ashx')
        params <- list(w=300, h=300, id=x)
        BiodbUrl(url=url, params=params)$toString()
    }

    return(vapply(id, fct, FUN.VALUE=''))
},

# Get all record fields {{{3
################################################################################

getAllRecordFields=function() {
    ":\n\nGets the complete list of all record fields provided by ChemSpider.
    \nReturned value: A character vector containing all the record fields.
    "

    return(c('SMILES', 'Formula', 'InChI', 'InChIKey', 'StdInChI',
             'StdInChIKey', 'AverageMass', 'MolecularWeight',
             'MonoisotopicMass', 'NominalMass', 'CommonName', 'ReferenceCount',
             'DataSourceCount', 'PubMedCount', 'RSCCount', 'Mol2D', 'Mol3D'))
},

# Web service records-recordId-details-get {{{3
################################################################################

wsRecordsRecordidDetailsGet=function(recordid, fields=NULL,
                                     retfmt=c('plain', 'parsed', 'request')) {
    ":\n\nAccesses the records-recordId-details-get ChemSpider web service. See
    https://developer.rsc.org/compounds-v1/apis/get/records/{recordId}/details
    for more details.
    \nrecordid: The ID of the record.
    \nfields: The list of fields to get.
    \nretfmt: The return format to use. 'plain' will return the exact results
    sent by the web service, as a character value. 'parsed' will return a JSON
    object. 'request' will return a BiodbRequest object containing the request
    that would have been sent.
    \nReturned value: Depending on `retfmt`.
    "

    retfmt <- match.arg(retfmt)

    # Convert ID to integer
    recordid <- suppressWarnings(as.integer(recordid))

    # Get fields to retrieve
    if (is.null(fields))
        fields <- paste(.self$getAllRecordFields(), collapse=',')

    # Build request
    header <- c('Content-Type'="", apikey=.self$getPropertyValue('token'))
    burl <- BiodbUrl(c(.self$getPropValSlot('urls', 'ws.url'), 'records',
                       recordid, 'details'), params=c(fields=fields))
    request <- BiodbRequest(method='get', url=burl, header=header)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Error
    if (is.null(results) || is.na(results))
        results <- NULL
    else if (retfmt == 'parsed')
        results <- jsonlite::fromJSON(results, simplifyDataFrame=FALSE)

    return(results)
},

# Web service records-batch-post {{{3
################################################################################

wsRecordsBatchPost=function(recordids, fields=NULL,
                            retfmt=c('plain', 'parsed', 'request')) {
    ":\n\nAccesswa the filter-name-post ChemSpider web service. See
    https://developer.rsc.org/compounds-v1/apis/post/records/batch.
    \nrecordids: A character vector containing record IDs.
    \nfields: The list of fields to get.
    \nretfmt: The return format to use. 'plain' will return the exact results
    sent by the web service, as a character value. 'parsed' will return a JSON
    object. 'request' will return a BiodbRequest object containing the request
    that would have been sent.
    \nReturned value: Depending on `retfmt`.
    "

    retfmt <- match.arg(retfmt)

    # Convert IDs to integer
    recordids <- suppressWarnings(as.integer(recordids))
    recordids <- recordids[ ! is.na(recordids)]

    # Get fields to retrieve
    if (is.null(fields))
        fields <- .self$getAllRecordFields()

    # Build request
    header <- c('Content-Type'="", apikey=.self$getPropertyValue('token'))
    sfields <- paste(vapply(fields, function(x) paste0('"', x, '"'),
                            FUN.VALUE=''), collapse=',')
    body <- paste0('{"recordIds": [', paste(recordids, collapse=','),
                   '], "fields": [', sfields ,']}')
    burl <- BiodbUrl(c(.self$getPropValSlot('urls', 'ws.url'),
                       'records', 'batch'))
    request <- BiodbRequest(method='post', url=burl, header=header, body=body)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Error
    if (is.null(results) || is.na(results))
        results <- NULL
    else if (retfmt == 'parsed')
        results <- jsonlite::fromJSON(results, simplifyDataFrame=FALSE)

    return(results)
},

# Web service filter-name-post {{{3
################################################################################

wsFilterNamePost=function(name, retfmt=c('plain', 'parsed', 'queryid', 'ids',
                                         'request')) {
    ":\n\nAccesses the filter-name-post ChemSpider web service. See
    https://developer.rsc.org/compounds-v1/apis/post/filter/name.
    \nname:
    \nretfmt: The return format to use. 'plain' will return the exact results
    sent by the web service, as a character value. 'parsed' will return a JSON
    object. 'request' will return a BiodbRequest object containing the request
    that would have been sent. 'queryid' will return a query ID usable with
    wsFilterQueryIdStatusGet() and wsFilterQueryIdResultsGet() in order to do
    asynchronous querying. 'ids' will return a character vector of entry IDs.
    \nReturned value: Depending on `retfmt`.
    "

    retfmt <- match.arg(retfmt)

    # Build request
    header <- c('Content-Type'="", apikey=.self$getPropertyValue('token'))
    body <- paste0("{\n", '\t"name": "', name, '"', "\n}")
    burl <- BiodbUrl(c(.self$getPropValSlot('urls', 'ws.url'),
                       'filter', 'name'))
    request <- BiodbRequest(method='post', url=burl, header=header, body=body)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Error
    if (is.null(results) || is.na(results))
        results <- NULL
    else
        results <- .self$.retrieveQuery(results=results, retfmt=retfmt)

    return(results)
},

# Web service filter-mass-post {{{3
################################################################################

wsFilterMassPost=function(mass, range, retfmt=c('plain', 'parsed', 'queryid',
                                                'ids', 'request')) {
    ":\n\nAccesses the filter-mass-post ChemSpider web service. See
    https://developer.rsc.org/compounds-v1/apis/post/filter/mass.
    \nmass: The mass to search for.
    \nrange: The range of the searched mass. Plain range, Dalton unit. The
    masses searched are between (mass - range) and (mass + range).
    \nretfmt: The return format to use. 'plain' will return the exact results
    sent by the web service, as a character value. 'parsed' will return a JSON
    object. 'request' will return a BiodbRequest object containing the request
    that would have been sent. 'queryid' will return a query ID usable with
    wsFilterQueryIdStatusGet() and wsFilterQueryIdResultsGet() in order to do
    asynchronous querying. 'ids' will return a character vector of entry IDs.
    \nReturned value: Depending on `retfmt`.
    "

    retfmt <- match.arg(retfmt)

    # Build request
    header <- c('Content-Type'="", apikey=.self$getPropertyValue('token'))
    body <- paste0("{\n", '\t"mass": ', mass, ",\n",'\t"range": ', range, "\n}")
    burl <- BiodbUrl(c(.self$getPropValSlot('urls', 'ws.url'),
                       'filter', 'mass'))
    request <- BiodbRequest(method='post', url=burl, header=header, body=body)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Error
    if (is.null(results) || is.na(results))
        results <- NULL
    else
        results <- .self$.retrieveQuery(results=results, retfmt=retfmt)

    return(results)
},

# Web service filter-queryId-status-get {{{3
################################################################################

wsFilterQueryIdStatusGet=function(queryid, retfmt=c('plain', 'parsed', 'status',
                                                    'request'),
                                  cache.read=FALSE) {
    ":\n\nAccesses the filter-queryId-status-get ChemSpider web service. See
    https://developer.rsc.org/compounds-v1/apis/get/filter/{queryId}/status.
    \nqueryid: A query ID previously obtained with a request like
    wsFilterNamePost().
    \nretfmt: The return format to use. 'plain' will return the exact results
    sent by the web service, as a character value. 'parsed' will return a JSON
    object. 'request' will return a BiodbRequest object containing the request
    that would have been sent. 'status' will return only the status.
    \ncache.read: If set to FALSE, the biodb cache won't be used. This is
    necessary for normal use of this web service, since the status needs to be
    polled on the server on each call.
    \nReturned value: Depending on `retfmt`.
    "

    .self$.assertNotNull(queryid)
    .self$.assertNotNa(queryid)
    .self$.assertIs(queryid, 'character')

    retfmt <- match.arg(retfmt)

    # Set URL
    header <- c('Content-Type'="", apikey=.self$getPropertyValue('token'))
    burl <- BiodbUrl(c(.self$getPropValSlot('urls', 'ws.url'), 'filter',
                       queryid, 'status'))
    request <- BiodbRequest(method='get', url=burl, header=header)
    if (retfmt == 'request')
        return(request)

    # Send request
    sched <- .self$getBiodb()$getRequestScheduler()
    results <- sched$sendRequest(request, cache.read=cache.read)

    # Parse JSON
    if (retfmt != 'plain') {

        results <- jsonlite::fromJSON(results, simplifyDataFrame=FALSE)

        # Get status
        if (retfmt == 'status')
            results <- results$status
    }

    return(results)
},

# Web service filter-queryId-results-get {{{3
################################################################################

wsFilterQueryIdResultsGet=function(queryid, start=0L, count=0L,
                                   retfmt=c('plain', 'parsed', 'ids',
                                            'request')) {
    ":\n\nAccesses the filter-queryId-results-get ChemSpider web service. See
    https://developer.rsc.org/compounds-v1/apis/get/filter/{queryId}/results.
    \nqueryid: A query ID previously obtained with a request like
    wsFilterNamePost().
    \nstart: The index of the first result to retrieve.
    \ncount: The number of results to retrieve.
    \nretfmt: The return format to use. 'plain' will return the exact results
    sent by the web service, as a character value. 'parsed' will return a JSON
    object. 'request' will return a BiodbRequest object containing the request
    that would have been sent. 'ids' will return a character vector of entry
    IDs.
    \nReturned value: Depending on `retfmt`.
    "

    .self$.assertNotNull(queryid)
    .self$.assertNotNa(queryid)
    .self$.assertIs(queryid, 'character')
    .self$.assertNumber(start, negative=FALSE, float.allowed=FALSE)
    .self$.assertNumber(count, negative=FALSE, float.allowed=FALSE)

    retfmt <- match.arg(retfmt)

    # Build request
    url <- BiodbUrl(c(.self$getPropValSlot('urls', 'ws.url'), 'filter', queryid,
                      'results'))
    if (start > 0)
        url$setParam('start', start)
    if (count > 0)
        url$setParam('count', count)
    header <- c('Content-Type'="", apikey=.self$getPropertyValue('token'))
    request <- BiodbRequest(method='get', url=url, header=header)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse JSON
    if (retfmt != 'plain') {

        results <- jsonlite::fromJSON(results, simplifyDataFrame=FALSE)

        # Parse IDs
        if (retfmt == 'ids')
            results <- results$results
    }

    return(results)
},

# Search compound {{{3
################################################################################

searchCompound=function(name=NULL, mass=NULL, mass.field=NULL, mass.tol=0.01,
                        mass.tol.unit='plain', max.results=NA_integer_) {
    # Overrides super class' method
        
    .self$.checkMassField(mass=mass, mass.field=mass.field)

    ids <- NULL

    # Send request on mass
    if ( ! is.null(mass) && ! is.null(mass.field)) {
        mass.field <- .self$getBiodb()$getEntryFields()$getRealName(mass.field)
        if (mass.field == 'monoisotopic.mass') {
            if (mass.tol.unit == 'ppm')
                range <- mass * mass.tol * 1.e-6
            else
                range <- mass.tol
            ids <- .self$wsFilterMassPost(mass=mass, range=range, retfmt='ids')
        }
        else
            .self$caution('Mass field "', mass.field, '" is not handled.')
    }

    # Search by name
    if ( ! is.null(name)) {

        name.id <- .self$wsFilterNamePost(name, retfmt='ids')

        # Merge with already found IDs
        if (is.null(ids))
            ids <- name.id
        else
            ids <- ids[ids %in% name.id]
    }

    # Cut
    if ( ! is.null(ids) && ! is.na(max.results) && max.results > 0
        && max.results < length(ids))
        ids <- ids[seq_len(max.results)]

    return(ids)
},

# Private methods {{{2
################################################################################

# Retrieve query {{{3
################################################################################

.retrieveQuery=function(results, retfmt) {

    # Parse JSON
    if (retfmt != 'plain') {

        results <- jsonlite::fromJSON(results, simplifyDataFrame=FALSE)

        if (retfmt == 'queryid')
            results <- results$queryId

        # Get results
        else if (retfmt == 'ids') {

            # Wait for query result to be ready
            cache.read <- TRUE
            while (TRUE) {
                status <- .self$wsFilterQueryIdStatusGet(results$queryId,
                                                         retfmt='status',
                                                         cache.read=cache.read)
                if (is.null(status))
                    return(NULL)

                if (status == 'Complete')
                    break

                cache.read <- FALSE
            }

            # Get results
            ids <- integer()
            while (TRUE) {

                res <- .self$wsFilterQueryIdResultsGet(results$queryId,
                                                       retfmt='parsed')
                ids <- c(ids, res$results)

                if ( ! res$limitedToMaxAllowed)
                    break
            }
            results <- as.character(ids)
        }
    }

    return(results)
},

# Get entry ids {{{3
################################################################################

.doGetEntryIds=function(max.results=NA_integer_) {

    .self$caution("Method using a last resort solution for its implementation.",
                  " Returns only a small subset of ChemSpider entries. This is",
                  " because ChemSpider API does not provide a service for",
                  " obtaining the exact number of entries.")

    ids <- NULL

    if ( ! is.na(max.results))
        mass.tol <- if (max.results <= 100) 0.01 else (0.01 * max.results / 100)
    else
        mass.tol <- 10
    ids <- .self$searchCompound(mass=100, mass.field='monoisotopic.mass',
                                mass.tol=mass.tol, max.results=max.results)

    return(ids)
},

# Do get entry content request {{{3
################################################################################

.doGetEntryContentRequest=function(id, concatenate=TRUE) {

    # Use batch requests
    if (concatenate) {

        # Divide IDs into group of max 100
        id.chunks <- split(id, ceiling(seq_along(id) / 100))

        # Create requests
        fct <- function(x) .self$wsRecordsBatchPost(x, retfmt='request')
        requests <- lapply(id.chunks, fct)
    }

    # One request for each ID
    else {
        fct <- function(x) .self$wsRecordsRecordidDetailsGet(x,
                                                             retfmt='request')
        requests <- lapply(id, fct)
    }

    return(requests)
}

))
