# vi: fdm=marker ts=4 et cc=80 tw=80

# ChebiConn {{{1
################################################################################

#' ChEBI connector class.
#'
#' This is the connector class for connecting to the ChEBI database through its
#' web services.
#'
#' @include BiodbCompounddbConn.R
#' @include BiodbSearchable.R
#' @include BiodbRemotedbConn.R
ChebiConn <- methods::setRefClass("ChebiConn",
    contains=c("BiodbRemotedbConn", "BiodbCompounddbConn", "BiodbSearchable"),

# Fields {{{2
################################################################################

fields=list(
    wsdl='ANY',
    ws.values='list' # Stores WSDL values
),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)

    .self$initFields(wsdl=NULL, ws.values=list())
},

# Get entry content request {{{3
################################################################################

.doGetEntryContentRequest=function(id, concatenate=TRUE) {
    
    url <- c(.self$getPropValSlot('urls', 'ws.url'), 'test',
             'getCompleteEntity')
    
    urls <- vapply(id, function(x) BiodbUrl(url=url,
                                            params=list(chebiId=x))$toString(),
                   FUN.VALUE='')
    
    return(urls)
},

# Get entry page url {{{3
################################################################################

getEntryPageUrl=function(id) {
    
    url <- c(.self$getPropValSlot('urls', 'base.url'), 'searchId.do')
    
    urls <- vapply(id, function(x) BiodbUrl(url=url,
                                            params=list(chebiId=x))$toString(),
                   FUN.VALUE='')
    
    return(urls)
},

# Get entry image url {{{3
################################################################################

getEntryImageUrl=function(id) {

    url <- c(.self$getPropValSlot('urls', 'base.url'), 'displayImage.do')
    
    urls <- vapply(id,
                   function(x) BiodbUrl(url=url,
                                        params=list(defaultImage='true',
                                          imageIndex=0, chebiId=x,
                                          dimensions=400))$toString(),
                  FUN.VALUE='')
    
    return(urls)
},


# Web service WSDL {{{3
################################################################################

wsWsdl=function(retfmt=c('plain', 'parsed', 'request')) {
    'Returns the complete WSDL from the the web server.'

    retfmt <- match.arg(retfmt)

    # Build request
    url <- c(.self$getPropValSlot('urls', 'ws.url'), 'webservice')
    request <- BiodbRequest(method='get', url=BiodbUrl(url=url, params='wsdl'))
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse
    if (retfmt == 'parsed')
        results <-  XML::xmlInternalTreeParse(results, asText=TRUE)

    return(results)
},

# Web service getLiteEntity {{{3
################################################################################

wsGetLiteEntity=function(search=NULL, search.category='ALL', max.results=10,
                         stars='ALL',
                         retfmt=c('plain', 'parsed', 'request', 'ids')) {
    'Calls getLiteEntity web service and returns the XML result.
    Be careful when search by mass
    (search.category="MASS" or "MONOISOTOPIC MASS"), since the searched is made
     in text mode, thus the number must be exactly written as it stored in
     database eventually padded with 0 in order to have exactly 5 digits after
     the decimal. An easy solution is to use wildcards to search a mass:
     "410;.718*".
    See http://www.ebi.ac.uk/chebi/webServices.do.'

    retfmt <- match.arg(retfmt)

    # Check parameters
    .self$.assertNotNull(search)
    .self$.assertNotNa(search)
    .self$.assertIn(search.category, .self$getSearchCategories())
    if (is.na(max.results))
        max.results <- 0
    .self$.assertPositive(max.results)
    .self$.assertIn(stars, .self$getStarsCategories())

    # Build request
    params <- c(search=search,
                searchCategory=search.category,
                maximumResults=max.results,
                starsCategory=stars)
    url <- c(.self$getPropValSlot('urls', 'ws.url'), 'test/getLiteEntity')
    request <- BiodbRequest(method='get', url=BiodbUrl(url=url, params=params),
                            encoding='UTF-8')
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse
    if (retfmt != 'plain') {

        # Parse XML
        results <-  XML::xmlInternalTreeParse(results, asText=TRUE)

        if (retfmt == 'ids') {
            ns <- .self$getPropertyValue('xml.ns')
            results <- XML::xpathSApply(results, "//chebi:chebiId",
                                        XML::xmlValue, namespaces=ns)
            results <- sub('CHEBI:', '', results)
            if (length(grep("^[0-9]+$", results)) != length(results))
                .self$error("Impossible to parse XML to get entry IDs.")
        }
    }

    return(results)
},

# Convert IDs to ChEBI IDs {{{3
################################################################################

convIdsToChebiIds=function(ids, search.category, simplify=TRUE) {
    'Convert a list of IDs into a list of ChEBI IDs. Several ChEBI IDs may
    be returned for a single ID. If `simplify` is set to `TRUE`, and only
    one ChEBI ID has been found for each ID, then a character vector is
    returned. Otherwise a list of character vectors is returned. The
    `search.category` parameter is the same used for `wsGetLiteEntity()`.'
    
    chebi <- list()
    msg <- paste('Converting', search.category, 'IDs to ChEBI IDs.')
    
    # Loop on all cas IDs
    i <- 0
    for (id in ids) {
        
        # Get ChEBI IDs for this ID
        if (is.na(id))
            x <- character()
        else
            x <- .self$wsGetLiteEntity(id, search.category=search.category,
                                       retfmt='ids')
        
        chebi <- c(chebi, list(x))
        
        # Send progress message
        i <- i + 1
        .self$progressMsg(msg=msg, index=i, total=length(ids), first=(i == 1))
    }
    
    # Simplify
    if (simplify && all(vapply(chebi, length, FUN.VALUE=1L) < 2)) {
        chebi <- lapply(chebi, function(x) if (length(x) == 0) NA_character_
                                           else x)
        chebi <- unlist(chebi)
    }

    return(chebi)
},

# Convert InChI to ChEBI ID {{{3
################################################################################

convInchiToChebi=function(inchi, simplify=TRUE) {
    'Convert a list of InChI or InChI KEYs into a list of ChEBI IDs. Several
    ChEBI IDs may
    be returned for a single InChI or InChI KEY. If `simplify` is set to `TRUE`,
    and only
    one ChEBI ID has been found for each InChI, then a character vector is
    returned. Otherwise a list of character vectors is returned.'
    
    return(.self$convIdsToChebiIds(inchi, search.category='INCHI/INCHI KEY',
                                   simplify=simplify))
},

# Convert CAS ID to ChEBI ID {{{3
################################################################################

convCasToChebi=function(cas, simplify=TRUE) {
    'Convert a list of CAS IDs into a list of ChEBI IDs. Several ChEBI IDs may
    be returned for a single CAS ID. If `simplify` is set to `TRUE`, and only
    one ChEBI ID has been found for each CAS ID, then a character vector is
    returned. Otherwise a list of character vectors is returned.'
    
    return(.self$convIdsToChebiIds(cas, search.category='REGISTRY NUMBERS',
                                   simplify=simplify))
},

# Search by name {{{3
################################################################################

searchByName=function(name, max.results=NA_integer_) {
    return(.self$searchCompound(name=name, max.results=max.results))
},

# Search compound {{{3
################################################################################

searchCompound=function(name=NULL, mass=NULL, mass.field=NULL, mass.tol=0.01,
                        mass.tol.unit='plain', max.results=NA_integer_) {
        
    .self$.checkMassField(mass=mass, mass.field=mass.field)

    ids <- NULL
    
    # Search by name
    if ( ! is.null(name))
        ids <- .self$wsGetLiteEntity(search=name, search.category="ALL NAMES",
                                     max.results=0, retfmt='ids')

    # Search by mass
    if ( ! is.null(mass) && ! is.null(mass.field)) {

        mass.field <- .self$getBiodb()$getEntryFields()$getRealName(mass.field)

        if ( ! mass.field %in% c('monoisotopic.mass' ,'molecular.mass'))
            .self$caution('Mass field "', mass.field, '" is not handled.')

        else {

            # Compute mass range
            if (mass.tol.unit == 'ppm') {
                mass.min <- mass * (1 - mass.tol * 1e-6)
                mass.max <- mass * (1 + mass.tol * 1e-6)
            } else {
                mass.min <- mass - mass.tol
                mass.max <- mass + mass.tol
            }

            # Search for masses
            if (is.null(ids)) {

                # Set search category
                search.category <- if (mass.field == 'monoisotopic.mass') 'MASS'
                    else 'MONOISOTOPIC MASS'

                # Search for all masses in the range
                range <- seq(as.integer(mass.min), as.integer(mass.max))
                for (integer.mass in range) {
                    x <- .self$wsGetLiteEntity(search=paste0(integer.mass, '*'),
                                               search.category=search.category,
                                               max.results=0, retfmt='ids')
                    ids <- c(ids, x)
                }

                # Remove duplicates
                ids <- ids[ ! duplicated(ids)]
            }
            
            # Filtering on mass range
            if ( ! is.null(ids)) {

                # Get masses of all entries
                entries <- .self$getEntry(ids, drop=FALSE)
                masses <- .self$getBiodb()$entriesToDataframe(entries,
                                                              compute=FALSE,
                                                              fields=mass.field,
                                                              drop=TRUE)

                # Filter on mass
                ids <- ids[(masses >= mass.min) & (masses <= mass.max)]
            }
        }
    }

    if (is.null(ids))
        ids <- character(0)

    # Cut
    if ( ! is.na(max.results) && max.results > 0 && max.results < length(ids))
        ids <- ids[seq_len(max.results)]

    return(ids)
},

# Get WSDL {{{3
################################################################################

getWsdl=function() {
    'Get the WSDL as a an XML object.'
    
    if (is.null(.self$wsdl))
        .self$wsdl <- .self$wsWsdl(retfmt='parsed')
    
    return(.self$wsdl)
},

# Get WSDL enumeration {{{3
################################################################################

getWsdlEnumeration=function(name) {
    'Extract a list of enumerations from the WSDL.'
    
    if ( ! name %in% names(.self$ws.values)) {

        ns <- .self$getPropertyValue('xml.ns')

        # Get search categories
        expr <- paste0("//xsd:simpleType[@name='", name, "']//xsd:enumeration")
        res <- XML::xpathSApply(.self$getWsdl(), expr, XML::xmlGetAttr, 'value',
                                namespaces=ns)
        .self$ws.values[[name]] <- res
    }
    
    return(.self$ws.values[[name]])
},

# Get stars categories {{{3
################################################################################

getStarsCategories=function() {
    'Returns the list of allowed stars categories for the getLiteEntity web
    service.'
    
    return(.self$getWsdlEnumeration('StarsCategory'))
},

# Get search categories {{{3
################################################################################

getSearchCategories=function() {
    'Returns the list of allowed search categories for the getLiteEntity web
    service.'

    return(.self$getWsdlEnumeration('SearchCategory'))
},

# Private methods {{{2
################################################################################

# Get entry ids {{{3
################################################################################

.doGetEntryIds=function(max.results=NA_integer_) {
    return(.self$wsGetLiteEntity(search='1*', search.category='CHEBI ID',
                                 max.results=max.results, retfmt='ids'))
}

))
