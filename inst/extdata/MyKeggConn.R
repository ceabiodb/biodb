MyKeggConn <- methods::setRefClass("MyKeggConn",
    contains="BiodbRemotedbConn",

methods=list(

# KEEP
# Overrides super class' method.
getEntryPageUrl=function(id) {
    
    u <- c(.self$getPropValSlot('urls', 'entry.page.url'), 'www_bget')
    id <- paste('cpd', id, sep=':')
    fct <- function(x) BiodbUrl(url=u, params=id)$toString()

    return(vapply(id, fct, FUN.VALUE=''))
},

# KEEP
# Overrides super class' method.
searchByName=function(name, max.results=NA_integer_) {

    ids <- NULL

    # Search by name
    if ( ! is.null(name) && ! is.na(name)) {

        # Build request
        u <- c(.self$getPropValSlot('urls', 'ws.url'), 'find', 'compound',
               name)
        url <- BiodbUrl(url=u)
        request <- .self$makeRequest(url=url)

        # Send request
        results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

        # Parse data frame
        readtc <- textConnection(results, "r", local=TRUE)
        df <- read.table(readtc, sep="\t", quote='', stringsAsFactors=FALSE)
        close(readtc)
        results <- df

        ids <- results[[1]]
        ids <- .self$wsFind(name, retfmt='ids')
        ids <- sub('^[^:]*:', '', ids)
    }

    # Cut
    if ( ! is.na(max.results) && max.results > 0 && max.results < length(ids))
        ids <- ids[seq_len(max.results)]

    return(ids)
},

# KEEP
# Overrides super class' method.
.doGetEntryContentRequest=function(id, concatenate=TRUE) {
    
    fct <- function(x) {
        u <- c(.self$getPropValSlot('urls', 'ws.url'), 'get', x)
        BiodbUrl(url=u)$toString()
    }
    
    return(vapply(id, fct, FUN.VALUE=''))
},

# KEEP
# Overrides super class' method.
.doGetEntryIds=function(max.results=NA_integer_) {

    # Build request
    u <- c(.self$getPropValSlot('urls', 'ws.url'), 'list', 'compound')
    url <- BiodbUrl(url=u)
    request <- .self$makeRequest(url=url)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Extract IDs
    results <- strsplit(results, "\n")[[1]]
    ids <- sub('^[^:]+:([^\\s]+)\\s.*$', '\\1', results, perl=TRUE)


    return(ids)
}

))
