# vi: fdm=marker ts=4 et cc=80 tw=80

# MirbaseMatureConn {{{1
################################################################################

#' @include MirbaseConn.R
#' @include BiodbDownloadable.R
MirbaseMatureConn <- methods::setRefClass("MirbaseMatureConn",
    contains=c("MirbaseConn", "BiodbDownloadable"),

# Public methods {{{2
################################################################################

methods=list(

# Get entry page url {{{3
################################################################################

getEntryPageUrl=function(id) {
    
    url <- c(.self$getPropValSlot('urls', 'base.url'), 'cgi-bin', 'mature.pl')
    v <- vapply(id,
                function(x) BiodbUrl(url=url,
                                     params=list(mature_acc=x))$toString(),
                FUN.VALUE='')
    
    return(v)
},

# Get entry image url {{{3
################################################################################

getEntryImageUrl=function(id) {
    return(rep(NA_character_, length(id)))
},

# Requires download {{{3
################################################################################

requiresDownload=function() {
    return(TRUE)
},

# Get entry content from database {{{3
################################################################################

getEntryContentFromDb=function(entry.id) {

    # Download
    .self$download()

    # Load content from cache
    cch <- .self$getBiodb()$getCache()
    ext <- .self$getPropertyValue('entry.content.type')
    content <- cch$loadFileContent(.self$getCacheId(), subfolder='shortterm',
                                   name=entry.id, ext=ext, output.vector=TRUE)

    return(content)
},

# Web service query {{{3
################################################################################

wsQuery=function(terms, submit='Search',
                 retfmt=c('plain', 'request', 'parsed', 'ids')) {
    "Send request to web service query."

    retfmt <- match.arg(retfmt)

    # Build request
    params <- list(terms=terms, submit=submit)
    url <- c(.self$getPropValSlot('urls', 'base.url'), 'cgi-bin', 'query.pl')
    url <- BiodbUrl(url=url, params=params)
    request <- BiodbRequest(url=url)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse results
    if (retfmt != 'plain') {

        # Parse HTML
        results <- XML::htmlTreeParse(results, asText=TRUE,
                                      useInternalNodes=TRUE)

        # Get IDs
        if (retfmt == 'ids') {
            results <- unlist(XML::xpathSApply(results,
                                               "//a[starts-with(.,'MIMAT')]",
                                               XML::xmlValue))
            if (is.null(results))
                results <- character()
        }
    }

    return(results)
},

# Search by name {{{3
################################################################################

searchByName=function(name, max.results=NA_integer_) {
        
    ids <- NULL

    # Search by name
    if ( ! is.null(name))
        ids <- .self$wsQuery(terms=name, retfmt='ids')

    # Cut
    if ( ! is.na(max.results) && max.results > 0 && max.results < length(ids))
        ids <- ids[seq_len(max.results)]

    return(ids)
},

# Private methods {{{2
################################################################################

# Do download {{{3
################################################################################

.doDownload=function() {

    url <- c(.self$getPropValSlot('urls', 'ftp.url'), 'mature.fa.gz')
    gz.url <- BiodbUrl(url=url)
    sched <- .self$getBiodb()$getRequestScheduler()
    sched$downloadFile(url=gz.url, dest.file=.self$getDownloadPath())
},

# Do extract download {{{3
################################################################################

.doExtractDownload=function() {

    # Extract
    # We do this because of the warning:
    # "seek on a gzfile connection returned an internal error"
    # when using `gzfile()`.
    extracted.file <- tempfile(.self$getId())
    R.utils::gunzip(filename=.self$getDownloadPath(), destname=extracted.file,
                    remove=FALSE)

    # Read file
    fd <- file(extracted.file, 'r')
    lines <- readLines(fd)
    close(fd)

    # Get all entry IDs
    ids <- sub('^.*(MIMAT[0-9]+).*$', '\\1', grep('MIMAT', lines, value=TRUE),
               perl=TRUE)
    .self$debug("Found ", length(ids), " entries in file \"",
                .self$getDownloadPath(), "\".")

    if (length(ids) > 0) {
        # Get contents
        contents <- paste(lines[seq(1, 2*length(ids), 2)],
                          lines[seq(2, 2*length(ids), 2)], sep="\n")

        # Write all entries into files
        cch <- .self$getBiodb()$getCache()
        cch$deleteFiles(.self$getCacheId(), subfolder='shortterm',
                        ext=.self$getPropertyValue('entry.content.type'))
        cch$saveContentToFile(contents, cache.id=.self$getCacheId(),
                              subfolder='shortterm', name=ids,
                              ext=.self$getPropertyValue('entry.content.type'))
    }

    # Remove extract directory
    unlink(extracted.file)
},

# Get entry ids {{{3
################################################################################

.doGetEntryIds=function(max.results=NA_integer_) {

    ids <- NULL

    # Download
    .self$download()

    # Get IDs from cache
    cch <- .self$getBiodb()$getCache()
    ids <- cch$listFiles(.self$getCacheId(), subfolder='shortterm',
                         ext=.self$getPropertyValue('entry.content.type'),
                         extract.name=TRUE)

    # Filter out wrong IDs
    ids <- ids[grepl("^MIMAT[0-9]+$", ids, perl=TRUE)]

    return(ids)
}

))
