# vi: fdm=marker ts=4 et cc=80 tw=80

# ChebiConn {{{1
################################################################################

# Declaration {{{2
################################################################################

#' ChEBI connector class.
#'
#' This is the connector class for connecting to the ChEBI database through its
#' web services.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('chebi')
#'
#' # Get an entry
#' e <- conn$getEntry('15440')
#'
#' # Convert an InChI KEY to a ChEBI identifier
#' conn$convInchiToChebi('YYGNTYWPHWGJRM-AAJYLUCBSA-N')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbCompounddbConn.R
#' @include BiodbRemotedbConn.R
#' @export ChebiConn
#' @exportClass ChebiConn
ChebiConn <- methods::setRefClass("ChebiConn",
    contains=c("BiodbRemotedbConn", "BiodbCompounddbConn"),
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

# Get entry page url {{{3
################################################################################

getEntryPageUrl=function(id) {
    # Overrides super class' method
    
    url <- c(.self$getPropValSlot('urls', 'base.url'), 'searchId.do')
    
    urls <- vapply(id, function(x) BiodbUrl(url=url,
                                            params=list(chebiId=x))$toString(),
                   FUN.VALUE='')
    
    return(urls)
},

# Get entry image url {{{3
################################################################################

getEntryImageUrl=function(id) {
    # Overrides super class' method

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
    ":\n\nRetrieves the complete WSDL from the web server.
    \nretfmt: The return format to use. 'plain' will return the value as it is
    returned by the server. 'parsed' will return an XML object. 'request' will
    return a BiodbRequest object representing the request that would have been
    sent. 
    \nReturned value: Depending on `retfmt` value.
    "

    retfmt <- match.arg(retfmt)

    # Build request
    url <- c(.self$getPropValSlot('urls', 'ws.url'), 'webservice')
    request <- .self$makeRequest(method='get', url=BiodbUrl(url=url,
                                                            params='wsdl'))
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
    ":\n\nCalls getLiteEntity web service and returns the XML result.
    Be careful when searching by mass
    (search.category='MASS' or 'MONOISOTOPIC MASS'), since the search is made
     in text mode, thus the number must be exactly written as it is stored in
     database, eventually padded with 0 in order to have exactly 5 digits after
     the decimal. An easy solution is to use wildcards to search a mass:
     '410;.718*'.
    See http://www.ebi.ac.uk/chebi/webServices.do for more details.
    \nsearch: The text or pattern to search.
    \nsearch.category: The search category. Call `getSearchCategories()`
    to get a full list of search categories.
    \nmax.results: The maximum of results to return.
    \nstars: How many starts the returned entities should have. Call
    `getStarsCategories() to get a full list of starts categories.`
    \nretfmt: The return format to use. 'plain' will return the results as given by the server, in a string. 'parsed' will return an XML object. 'request' will return a BiodbRequest object representing the request as would have been sent. 'ids' will return a list of matched entity IDs.
    \nReturned value: Depending on `retfmt` value.
    "

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
    request <- .self$makeRequest(method='get', url=BiodbUrl(url=url, params=params),
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
    ":\n\nConverts a list of IDs (InChI, InChI Keys, CAS, ...) into a list of
    ChEBI IDs. Several ChEBI IDs may be returned for a single ID.
    \nsimplify: If set to TRUE and only one ChEBI ID has been found for each ID,
    then a character vector is returned. Otherwise a list of character vectors
    is returned.
    \nsearch.category: The search category. Call `getSearchCategories()`
    to get a full list of search categories.
    \nReturned value: Depending on the value of simplify.
    "
    
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
    ":\n\nConverts a list of InChI or InChI KEYs into a list of ChEBI IDs.
    Several ChEBI IDs may be returned for a single InChI or InChI KEY.
    \nsimplify: If set to TRUE and only one ChEBI ID has been found for each ID,
    then a character vector is returned. Otherwise a list of character vectors
    is returned.
    \nReturned value: Depending on the value of simplify.
    "
    
    return(.self$convIdsToChebiIds(inchi, search.category='INCHI/INCHI KEY',
                                   simplify=simplify))
},

# Convert CAS ID to ChEBI ID {{{3
################################################################################

convCasToChebi=function(cas, simplify=TRUE) {
    ":\n\nConverts a list of CAS IDs into a list of ChEBI IDs.
    Several ChEBI IDs may be returned for a single InChI or InChI KEY.
    \nsimplify: If set to TRUE and only one ChEBI ID has been found for each ID,
    then a character vector is returned. Otherwise a list of character vectors
    is returned.
    \nReturned value: Depending on the value of simplify.
    "
    
    return(.self$convIdsToChebiIds(cas, search.category='REGISTRY NUMBERS',
                                   simplify=simplify))
},

# Search compound {{{3
################################################################################

searchCompound=function(name=NULL, mass=NULL, mass.field=NULL, mass.tol=0.01,
                        mass.tol.unit='plain', max.results=NA_integer_) {
    # Overrides super class' method.

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

            # Filtering on mass range after name search
            if ( ! is.null(ids))
                ids <- .self$.filterIdsOnMassRange(ids, mass.min, mass.max,
                                                   mass.field, max.results)
            
            # Pure mass search
            else {

                # Set search category
                search.category <- if (mass.field == 'monoisotopic.mass')
                    'MONOISOTOPIC MASS' else 'MASS'

                # Search for all masses in the range
                n <- floor(log10(mass.max - mass.min))
                if (n >= 0)
                    n <- -1
                firstMass <- floor(mass.min * 10^-n) * 10^n
                lastMass <- ceiling(mass.max * 10^-n) * 10^n
                ids <- character()
                for (m in seq(firstMass, lastMass, 10^n)) {

                    # Get entries matching integer mass
                    x <- .self$wsGetLiteEntity(search=paste0(m, '*'),
                                               search.category=search.category,
                                               max.results=0, retfmt='ids')
                    
                    # Remove IDs that we already have
                    x <- x[ ! x %in% ids]
                    
                    # Filter on mass range
                    x <- .self$.filterIdsOnMassRange(x, mass.min, mass.max,
                                                     mass.field,
                                                     max.results - length(ids))
                    
                    # Add IDs
                    ids <- c(ids, x)
                    
                    if ( ! is.na(max.results) && max.results > 0 && length(ids)
                        >= max.results)
                        break
                }

                # Remove duplicates
                ids <- ids[ ! duplicated(ids)]
            }

        }
    }

    if (is.null(ids))
        ids <- character()

    # Cut
    if ( ! is.na(max.results) && max.results > 0 && max.results < length(ids))
        ids <- ids[seq_len(max.results)]

    return(ids)
},

# Get WSDL {{{3
################################################################################

getWsdl=function() {
    ":\n\nGets the WSDL as an XML object.
    \nReturned value: The ChEBI WSDL as an XML object.
    "
    
    if (is.null(.self$wsdl))
        .self$wsdl <- .self$wsWsdl(retfmt='parsed')
    
    return(.self$wsdl)
},

# Get WSDL enumeration {{{3
################################################################################

getWsdlEnumeration=function(name) {
    ":\n\nExtracts a list of values from an enumeration in the WSDL.
    \nname: The name of the enumeration for which to retrieve the values.
    \nReturned value: A character vector listing the enumerated values.
    "
    
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
    ":\n\nGets the list of allowed stars categories for the getLiteEntity web
    service.
    \nReturned value: Returns all the possible stars categories as a character
    vector.
    "
    
    return(.self$getWsdlEnumeration('StarsCategory'))
},

# Get search categories {{{3
################################################################################

getSearchCategories=function() {
    ":\n\nGets the list of allowed search categories for the getLiteEntity web
    service.
    \nReturned value: Returns all the possible search categories as a character
    vector.
    "

    return(.self$getWsdlEnumeration('SearchCategory'))
},

# Private methods {{{2
################################################################################

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

# Get entry ids {{{3
################################################################################

.doGetEntryIds=function(max.results=NA_integer_) {
    return(.self$wsGetLiteEntity(search='1*', search.category='CHEBI ID',
                                 max.results=max.results, retfmt='ids'))
},

# Filtering IDs on mass range {{{3
################################################################################

.filterIdsOnMassRange=function(ids, mass.min, mass.max, mass.field, limit) {

    retids <- character()
    msg <- paste0('Filtering ChEBI entries on mass range [', mass.min, ',
                  ', mass.max, '] and field "', mass.field, '".')

    # Loop on all IDs
    i <- 0
    for (id in ids) {

        # Print progress
        i <- i + 1
        .self$progressMsg(msg=msg, index=i, total=length(ids), first=(i == 1))

        # Get entry
        e <- .self$getEntry(id, drop=TRUE)
        
        # Test mass
        if ( ! is.null(e)) {
            m <- e$getFieldValue(mass.field)
            if ( ! is.na(m) && m >= mass.min && m <= mass.max) {
                retids <- c(retids, id)

                # Stop if limit is reached
                if ( ! is.na(limit) && limit > 0 && length(retids) >= limit)
                    break
            }
        }
    }

    return(retids)
}

))
