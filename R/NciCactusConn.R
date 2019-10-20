# vi: fdm=marker ts=4 et cc=80 tw=80

# NciCactusConn {{{1
################################################################################

# Declaration {{{2
################################################################################

#' NCI CACTUS connector class.
#'
#' This class implements a connector for accessing the NCI database, using
#' CACTUS services.  See https://www.cancer.gov/ and
#' https://cactus.nci.nih.gov/.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('nci.cactus')
#'
#' # Get an entry
#' e <- conn$getEntry('749674')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbRemotedbConn.R
#' @include BiodbDownloadable.R
#' @export NciCactusConn
#' @exportClass NciCactusConn
NciCactusConn <- methods::setRefClass("NciCactusConn",
    contains=c("BiodbRemotedbConn", "BiodbDownloadable"),

# Public methods {{{2
################################################################################

methods=list(

# Get nb entries {{{3
################################################################################

getNbEntries=function(count=FALSE) {
    # Overrides super class' method.

    n <- NA_integer_

    ids <- .self$getEntryIds()
    if ( ! is.null(ids))
        n <- length(ids)

    return(n)
},

# Get entry page url {{{3
################################################################################

getEntryPageUrl=function(id) {
    # Overrides super class' method.

    return(rep(NA_character_, length(id)))
},


# Web service Chemical Identifier Resolver {{{3
################################################################################

wsChemicalIdentifierResolver=function(structid, repr, xml=FALSE,
                                      retfmt=c('plain', 'parsed', 'request',
                                               'ids')) {
    ":\n\nCalls Chemical Identifier Resolver web service.
    See https://cactus.nci.nih.gov/chemical/structure_documentation for details.
    \nstructid: The submitted structure identifier.
    \nrepr: The wanted representation.
    \nxml: A flag for choosing the format returned by the web service between
    plain text and XML.
    \nretfmt: Use to set the format of the returned value. 'plain' will return
    the raw results from the server, as a character value. 'parsed' will return
    the parsed results, as an XML object. 'request' will return a BiodbRequest
    object representing the request as it would have been sent. 'ids' will
    return a character vector containing the IDs of the matching entries.
    \nReturned value: Depending on `retfmt` parameter.
    "

    retfmt <- match.arg(retfmt)
    
    # Build request
    url <- c(.self$getPropValSlot('urls', 'ws.url'), 'chemical', 'structure',
             structid, repr)
    if (xml)
        url <- c(url, 'xml')
    request <- .self$makeRequest(method='get', url=BiodbUrl(url=url))
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse
    if (retfmt != 'plain' && xml) {

        # Parse XML
        results <-  XML::xmlInternalTreeParse(results, asText=TRUE)

        if (retfmt == 'ids') {
            results <- XML::xpathSApply(results, "//item", XML::xmlValue)
            if (is.list(results)
                && all(vapply(results, is.null, FUN.VALUE=TRUE)))
                results <- character()
        }
    }

    return(results)
},

# Convert a list of IDs into another {{{3
################################################################################

conv=function(ids, repr) {
    ":\n\nCalls wsChemicalIdentifierResolver() to convert a list of IDs into
    another representation.
    \nids: A character vector containing IDs.
    \nrepr: The targeted representation.
    \nReturned value: A character vector, the same length as `ids`, containing
    the converted IDs. NA values will be set when conversion is not possible.
    "
    
    res <- character()
    msg <- paste0('Converting IDs to ', repr)
    
    # Loop on all IDs
    i <- 0
    for (id in ids) {
        r <- .self$wsChemicalIdentifierResolver(structid=id, repr=repr,
                                                xml=TRUE, retfmt='ids')
        if (length(r) == 0)
            r <- NA_character_
        
        res <- c(res, r)
        
        # Send progress message
        i <- i + 1
        .self$progressMsg(msg=msg, index=i, total=length(ids), first=(i == 1))
    }
    
    return(res)
},

# Convert CAS ID to InChI {{{3
################################################################################

convCasToInchi=function(cas) {
    ":\n\nConverts a list of CAS IDs into a list of InChI.
    \ncas: A character vector containing CAS IDs.
    \nReturned value: A character vector, the same length as `ids`, containing InChI values or NA values where conversion was not possible.
    "
    
    return(.self$conv(cas, 'InChI'))
},

# Convert CAS ID to InChI KEY {{{3
################################################################################

convCasToInchikey=function(cas) {
    ":\n\nConverts a list of CAS IDs into a list of InChI keys.
    \ncas: A character vector containing CAS IDs.
    \nReturned value: A character vector, the same length as `ids`, containing InChI Key values or NA values where conversion was not possible.
    "
    
    inchikey <- .self$conv(cas, 'InChIKEY')
    inchikey <- sub('^InChIKey=', '', inchikey)
                    
    return(inchikey)
},

# Requires download {{{3
################################################################################

requiresDownload=function() {
    # Overrides super class' method.

    return(TRUE)
},

# Get entry content from database {{{3
################################################################################

getEntryContentFromDb=function(entry.id) {
    # Overrides super class' method.

    # Initialize return values
    content <- rep(NA_character_, length(entry.id))

    return(content)
},

# Private methods {{{2
################################################################################

# Do download {{{3
################################################################################

.doDownload=function() {
    url <- c(.self$getPropValSlot('urls', 'dwnld.url'),
             'NCI-Open_2012-05-01.sdf.gz')
    gz.url <- BiodbUrl(url=url)
    sched <- .self$getBiodb()$getRequestScheduler()
    sched$downloadFile(url=gz.url, dest.file=.self$getDownloadPath())
},

# Do extract download {{{3
################################################################################

.doExtractDownload=function() {

    # Open compressed file
    fd <- gzfile(.self$getDownloadPath(), 'r')

    # Remove current entry files
    cch <- .self$getBiodb()$getPersistentCache()
    ect <- .self$getPropertyValue('entry.content.type')
    cch$deleteFiles(.self$getCacheId(), ext=ect)

    # Read all file content,
    # and extract all individual SDF files.
    content <- character()
    i <- 0
    msg <- 'Extracting NCI CACTUS SDF entry files'
    while(TRUE) {

        # Read one line
        line <- readLines(fd, n=1)
        if (length(line) == 0)
            break
        
        # Store line
        content <- c(content, line)
        
        # End of individual file
        if (line == '$$$$') {
            id <- as.integer(content[[1]])
            content <- paste(content, collapse="\n")
            cch$saveContentToFile(content, cache.id=.self$getCacheId(),
                                  name=id, ext=ect)
            content <- character()
            i <- i + 1
            .self$progressMsg(msg=msg, index=i, first=(i == 1))
        }
    }

    # Close file
    close(fd)
},

# Get entry ids {{{3
################################################################################

.doGetEntryIds=function(max.results=NA_integer_) {

    ids <- NULL

    # Download
    .self$download()

    # Get IDs from cache
    cch <- .self$getBiodb()$getPersistentCache()
    ids <- cch$listFiles(.self$getCacheId(),
                         ext=.self$getPropertyValue('entry.content.type'),
                         extract.name=TRUE)

    return(ids)
}

))
