# vi: fdm=marker ts=4 et cc=80

# Constants {{{1
# Class declaration {{{1
################################################################################

#' @include BiodbCompounddbConn.R
#' @include BiodbSearchable.R
#' @include BiodbRemotedbConn.R
ChebiConn <- methods::setRefClass("ChebiConn", contains = c("BiodbRemotedbConn", "BiodbCompounddbConn", "BiodbSearchable"), fields = list(.ws.values = 'list'))

# Initialize {{{1
################################################################################

ChebiConn$methods( initialize = function(...) {

    callSuper(...)

    .self$.ws.values <- list()
})

# Get entry content request {{{1
################################################################################

ChebiConn$methods( .doGetEntryContentRequest = function(id, concatenate = TRUE) {
    return(vapply(id, function(x) BiodbUrl(url = c(.self$getPropValSlot('urls', 'ws.url'), 'test', 'getCompleteEntity'), params = list(chebiId = x))$toString(), FUN.VALUE = ''))
})

# Get entry page url {{{1
################################################################################

ChebiConn$methods( getEntryPageUrl = function(id) {
    return(vapply(id, function(x) BiodbUrl(url = c(.self$getPropValSlot('urls', 'base.url'), 'searchId.do'), params = list(chebiId = x))$toString(), FUN.VALUE = ''))
})

# Get entry image url {{{1
################################################################################

ChebiConn$methods( getEntryImageUrl = function(id) {
    return(vapply(id, function(x) BiodbUrl(url = c(.self$getPropValSlot('urls', 'base.url'), 'displayImage.do'), params = list(defaultImage = 'true', imageIndex = 0, chebiId = x, dimensions = 400))$toString(), FUN.VALUE = ''))
})


# Web service WSDL {{{1
################################################################################

ChebiConn$methods( ws.wsdl = function(retfmt = c('plain', 'parsed', 'request')) {

    retfmt = match.arg(retfmt)

    # Build request
    request = BiodbRequest(method = 'get', url = BiodbUrl(url = c(.self$getPropValSlot('urls', 'ws.url'), 'webservice'), params = 'wsdl'))
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse
    if (retfmt == 'parsed')
        results <-  XML::xmlInternalTreeParse(results, asText = TRUE)

    return(results)
})

# Web service getLiteEntity {{{1
################################################################################

ChebiConn$methods( ws.getLiteEntity = function(search = NULL, search.category = 'ALL', max.results = 10, stars = 'ALL', retfmt = c('plain', 'parsed', 'request', 'ids')) {
    "Calls getLiteEntity web service and returns the XML result. See http://www.ebi.ac.uk/chebi/webServices.do. Be careful when search by mass (search.category = 'MASS' or 'MONOISOTOPIC MASS', since the searched is made in text mode, thus the number must be exactly written as it stored in database eventually padded with 0 in order to have exactly 5 digits after the decimal. An easy solution is to use wildcards to search a mass: '410;.718*'."

    retfmt = match.arg(retfmt)

    .self$.parseWebServiceValues()

    # Check parameters
    .self$.assert.not.null(search)
    .self$.assert.not.na(search)
    .self$.assert.in(search.category, .self$.ws.values$search.categories)
    if (is.na(max.results))
        max.results <- 0
    .self$.assert.positive(max.results)
    .self$.assert.in(stars, .self$.ws.values$stars.categories)

    # Build request
    params <- c(search = gsub('[ /]', '+', search), searchCategory = gsub(' ', '+', search.category), maximumResults = max.results, starsCategory = gsub(' ', '+', stars))
    request = BiodbRequest(method = 'get', url = BiodbUrl(url = c(.self$getPropValSlot('urls', 'ws.url'), 'test/getLiteEntity'), params = params), encoding = 'UTF-8')
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse
    if (retfmt != 'plain') {

        # Parse XML
        results <-  XML::xmlInternalTreeParse(results, asText = TRUE)

        if (retfmt == 'ids') {
            results <- XML::xpathSApply(results, "//chebi:chebiId", XML::xmlValue, namespaces = .self$getPropertyValue('xml.ns'))
            results <- sub('CHEBI:', '', results)
            if (length(grep("^[0-9]+$", results)) != length(results))
                .self$message('error', "Impossible to parse XML to get entry IDs.")
        }
    }

    return(results)
})

# Search by name {{{1
################################################################################

ChebiConn$methods( searchByName = function(name, max.results = NA_integer_) {
    return(.self$searchCompound(name = name, max.results = max.results))
})

# Search compound {{{1
################################################################################

ChebiConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.field = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {
        
    .self$.checkMassField(mass = mass, mass.field = mass.field)

    ids <- NULL
    
    # Search by name
    if ( ! is.null(name))
        ids <- .self$ws.getLiteEntity(search = name, search.category = "ALL NAMES", max.results = 0, retfmt = 'ids')

    # Search by mass
    if ( ! is.null(mass) && ! is.null(mass.field)) {

        mass.field <- .self$getBiodb()$getEntryFields()$getRealName(mass.field)

        if ( ! mass.field %in% c('monoisotopic.mass' ,'molecular.mass'))
            .self$message('caution', paste0('Mass field "', mass.field, '" is not handled.'))

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
                search.category <- if (mass.field == 'monoisotopic.mass') 'MASS' else 'MONOISOTOPIC MASS'

                # Search for all masses in the range
                for (integer.mass in seq(as.integer(mass.min), as.integer(mass.max)))
                    ids <- c(ids, .self$ws.getLiteEntity(search = paste0(integer.mass, '*'), search.category = search.category, max.results = 0, retfmt = 'ids'))

                # Remove duplicates
                ids <- ids[ ! duplicated(ids)]
            }
            
            # Filtering on mass range
            if ( ! is.null(ids)) {

                # Get masses of all entries
                entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), ids, drop = FALSE)
                masses <- .self$getBiodb()$entriesToDataframe(entries, compute = FALSE, fields = mass.field, drop = TRUE)

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
})

# Private methods {{{1
################################################################################

# Parse web service values {{{2
################################################################################

ChebiConn$methods( .parseWebServiceValues = function() {

    if (length(.self$.ws.values) == 0) {

        wsdl = .self$ws.wsdl(retfmt = 'parsed')

        # Get search categories
        .self$.ws.values$search.categories = XML::xpathSApply(wsdl, "//xsd:simpleType[@name='SearchCategory']//xsd:enumeration", XML::xmlGetAttr, 'value', namespaces = .self$getPropertyValue('xml.ns'))

        # Get stars categories
        .self$.ws.values$stars.categories = XML::xpathSApply(wsdl, "//xsd:simpleType[@name='StarsCategory']//xsd:enumeration", XML::xmlGetAttr, 'value', namespaces = .self$getPropertyValue('xml.ns'))
    }
})

# Get entry ids {{{2
################################################################################

ChebiConn$methods( .doGetEntryIds = function(max.results = NA_integer_) {
    return(.self$ws.getLiteEntity(search = '1*', search.category = 'CHEBI ID', max.results = max.results, retfmt = 'ids'))
})

