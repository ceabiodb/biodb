# vi: fdm=marker ts=4 et cc=80

# Class declaration {{{1
################################################################################

#' @include MirbaseConn.R
#' @include BiodbDownloadable.R
#' @include BiodbSearchable.R
MirbaseMatureConn <- methods::setRefClass("MirbaseMatureConn", contains = c("MirbaseConn", "BiodbDownloadable", "BiodbSearchable"))

# Initialize {{{1
################################################################################

MirbaseMatureConn$methods( initialize = function(...) {
    callSuper(...)

    .self$.setDownloadExt('gz')
})

# Get entry page url {{{1
################################################################################

MirbaseMatureConn$methods( getEntryPageUrl = function(id) {
    return(vapply(id, function(x) BiodbUrl(url = c(.self$getPropValSlot('urls', 'base.url'), 'cgi-bin', 'mature.pl'), params = list(mature_acc = x))$toString(), FUN.VALUE = ''))
})

# Get entry image url {{{1
################################################################################

MirbaseMatureConn$methods( getEntryImageUrl = function(id) {
    return(rep(NA_character_, length(id)))
})

# Requires download {{{1
################################################################################

MirbaseMatureConn$methods( requiresDownload = function() {
    return(TRUE)
})

# Do download {{{1
################################################################################

MirbaseMatureConn$methods( .doDownload = function() {

    # Download
    gz.url <- BiodbUrl(url = paste0(.self$getPropValSlot('urls', 'ftp.url'), 'mature.fa.gz'))
    .self$message('info', paste("Downloading \"", gz.url$toString(), "\"...", sep = ''))
    .self$getBiodb()$getRequestScheduler()$downloadFile(url = gz.url, dest.file = .self$getDownloadPath())
    .self$message('debug', 'Finish downloading Mirbase Mature database.')
})

# Do extract download {{{1
################################################################################

MirbaseMatureConn$methods( .doExtractDownload = function() {

    # Extract
    # We do this because of the warning "seek on a gzfile connection returned an internal error" when using `gzfile()`.
    extracted.file <- tempfile(.self$getId())
    R.utils::gunzip(filename = .self$getDownloadPath(), destname = extracted.file, remove = FALSE)

    # Read file
    fd <- file(extracted.file, 'r')
    lines <- readLines(fd)
    close(fd)

    # Get all entry IDs
    ids <- sub('^.*(MIMAT[0-9]+).*$', '\\1', grep('MIMAT', lines, value = TRUE), perl = TRUE)
    .self$message('debug', paste("Found ", length(ids), " entries in file \"", .self$getDownloadPath(), "\".", sep = ''))

    if (length(ids) > 0) {
        # Get contents
        contents <- paste(lines[seq(1, 2*length(ids), 2)], lines[seq(2, 2*length(ids), 2)], sep = "\n")

        # Write all entries into files
        .self$getBiodb()$getCache()$deleteFiles(.self$getCacheId(), subfolder = 'shortterm', ext = .self$getPropertyValue('entry.content.type'))
        .self$getBiodb()$getCache()$saveContentToFile(contents, cache.id = .self$getCacheId(), subfolder = 'shortterm', name = ids, ext = .self$getPropertyValue('entry.content.type'))
    }

    # Remove extract directory
    unlink(extracted.file)
})

# Get entry content from database {{{1
################################################################################

MirbaseMatureConn$methods( getEntryContentFromDb = function(entry.id) {

    # Download
    .self$download()

    # Load content from cache
    content <- .self$getBiodb()$getCache()$loadFileContent(.self$getCacheId(), subfolder = 'shortterm', name = entry.id, ext = .self$getPropertyValue('entry.content.type'), output.vector = TRUE)

    return(content)
})

# Web service query {{{1
################################################################################

MirbaseMatureConn$methods( ws.query = function(terms, submit = 'Search', retfmt = c('plain', 'request', 'parsed', 'ids')) {
    "Send request to web service query."

    retfmt = match.arg(retfmt)

    # Build request
    params = list(terms = terms, submit = submit)
    url = BiodbUrl(url = c(.self$getPropValSlot('urls', 'base.url'), 'cgi-bin', 'query.pl'), params = params)
    request = BiodbRequest(url = url)
    if (retfmt == 'request')
        return(request)

    # Send request
    results = .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse results
    if (retfmt != 'plain') {

        # Parse HTML
        results <-  XML::htmlTreeParse(results, asText = TRUE, useInternalNodes = TRUE)

        # Get IDs
        if (retfmt == 'ids') {
            results <- unlist(XML::xpathSApply(results, "//a[starts-with(.,'MIMAT')]", XML::xmlValue))
            if (is.null(results))
                results <- character()
        }
    }

    return(results)
})

# Search by name {{{1
################################################################################

MirbaseMatureConn$methods( searchByName = function(name, max.results = NA_integer_) {
        
    ids <- NULL

    # Search by name
    if ( ! is.null(name))
        ids <- .self$ws.query(terms = name, retfmt = 'ids')

    # Cut
    if ( ! is.na(max.results) && max.results > 0 && max.results < length(ids))
        ids <- ids[seq_len(max.results)]

    return(ids)
})

# Private methods {{{1
################################################################################

# Get entry ids {{{2
################################################################################

MirbaseMatureConn$methods( .doGetEntryIds = function(max.results = NA_integer_) {

    ids <- NULL

    # Download
    .self$download()

    # Get IDs from cache
    ids <- .self$getBiodb()$getCache()$listFiles(.self$getCacheId(), subfolder = 'shortterm', ext = .self$getPropertyValue('entry.content.type'), extract.name = TRUE)

    # Filter out wrong IDs
    ids <- ids[grepl("^MIMAT[0-9]+$", ids, perl = TRUE)]

    return(ids)
})

