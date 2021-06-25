ChebiExConn <- methods::setRefClass("ChebiExConn",
    contains="BiodbConn",

methods=list(

initialize=function(...) {
    callSuper(...)
},

getEntryPageUrl=function(id) {
    # Overrides super class' method

    url <- c(.self$getPropValSlot('urls', 'base.url'), 'searchId.do')

    fct <- function(x) {
        BiodbUrl$new(url=url, params=list(chebiId=x))$toString()
    }
    
    urls <- vapply(id, fct, FUN.VALUE='')

    return(urls)
},

getEntryImageUrl=function(id) {
    # Overrides super class' method

    url <- c(.self$getPropValSlot('urls', 'base.url'), 'displayImage.do')

    fct <- function(x) {
        BiodbUrl$new(url=url, params=list(defaultImage='true', imageIndex=0,
                                      chebiId=x, dimensions=400))$toString()
    }
    
    urls <- vapply(id, fct, FUN.VALUE='')

    return(urls)
},

wsGetLiteEntity=function(search=NULL, search.category='ALL', stars='ALL',
                         max.results=10,
                         retfmt=c('plain', 'parsed', 'request', 'ids')) {

    # Check parameters
    chk::chk_string(search)
    chk::chk_in(search.category, .self$getSearchCategories())
    chk::chk_number(max.results)
    chk::chk_gte(max.results, 0)
    chk::chk_in(stars, .self$getStarsCategories())
    retfmt <- match.arg(retfmt)

    # Build request
    params <- c(search=search,
                searchCategory=search.category,
                maximumResults=max.results,
                starsCategory=stars)
    url <- c(.self$getPropValSlot('urls', 'ws.url'), 'test/getLiteEntity')
    request <- .self$makeRequest(method='get', url=BiodbUrl$new(url=url,
                                                                params=params),
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

.doSearchForEntries=function(fields=NULL, max.results=0) {

    ids <- character()

    if ( ! is.null(fields)) {

        # Search by name
        if ('name' %in% names(fields))
            ids <- .self$wsGetLiteEntity(search=fields$name,
                                         search.category="ALL NAMES",
                                         max.results=0, retfmt='ids')
    }

    # Cut
    if (max.results > 0 && max.results < length(ids))
        ids <- ids[seq_len(max.results)]

    return(ids)
},

.doGetEntryContentRequest=function(id, concatenate=TRUE) {

    url <- c(.self$getPropValSlot('urls', 'ws.url'), 'test',
             'getCompleteEntity')

    urls <- vapply(id, function(x) BiodbUrl$new(url=url,
                                            params=list(chebiId=x))$toString(),
                   FUN.VALUE='')

    return(urls)
},

.doGetEntryIds=function(max.results=NA_integer_) {
    return(NULL)
}
))
