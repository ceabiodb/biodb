ChebiExConn <- methods::setRefClass("ChebiExConn",
    contains=c("BiodbRemotedbConn", "BiodbCompounddbConn"),

methods=list(

initialize=function(...) {
    callSuper(...)
},

getEntryPageUrl=function(id) {
    # Overrides super class' method

    url <- c(.self$getPropValSlot('urls', 'base.url'), 'searchId.do')

    fct <- function(x) {
        BiodbUrl(url=url, params=list(chebiId=x))$toString()
    }
    
    urls <- vapply(id, fct, FUN.VALUE='')

    return(urls)
},

getEntryImageUrl=function(id) {
    # Overrides super class' method

    url <- c(.self$getPropValSlot('urls', 'base.url'), 'displayImage.do')

    fct <- function(x) {
        BiodbUrl(url=url, params=list(defaultImage='true', imageIndex=0,
                                      chebiId=x, dimensions=400))$toString()
    }
    
    urls <- vapply(id, fct, FUN.VALUE='')

    return(urls)
},

wsGetLiteEntity=function(search=NULL, search.category='ALL', stars='ALL',
                         max.results=10,
                         retfmt=c('plain', 'parsed', 'request', 'ids')) {

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

searchCompound=function(name=NULL, mass=NULL, mass.field=NULL, mass.tol=0.01,
                        mass.tol.unit='plain', max.results=NA_integer_) {
    # Overrides super class' method.

    .self$.checkMassField(mass=mass, mass.field=mass.field)

    ids <- NULL

    # Search by name
    if ( ! is.null(name))
        ids <- .self$wsGetLiteEntity(search=name, search.category="ALL NAMES",
                                     max.results=0, retfmt='ids')


    # Search by mass not implemented for this example
    if ( ! is.null(mass) && ! is.null(mass.field))
        .self$caution('Database ', .self$getDbClass(),
                      ' is not searchable by mass.')

    # Return empty vector instead of NULL
    if (is.null(ids))
        ids <- character()

    # Cut
    if ( ! is.na(max.results) && max.results > 0 && max.results < length(ids))
        ids <- ids[seq_len(max.results)]

    return(ids)
},

.doGetEntryContentRequest=function(id, concatenate=TRUE) {

    url <- c(.self$getPropValSlot('urls', 'ws.url'), 'test',
             'getCompleteEntity')

    urls <- vapply(id, function(x) BiodbUrl(url=url,
                                            params=list(chebiId=x))$toString(),
                   FUN.VALUE='')

    return(urls)
},

.doGetEntryIds=function(max.results=NA_integer_) {
    return(NULL)
}
))
