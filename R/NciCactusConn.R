# vi: fdm=marker ts=4 et cc=80

# NciCactusConn {{{1
################################################################################

#' @include BiodbRemotedbConn.R
#' @include BiodbDownloadable.R
NciCactusConn <- methods::setRefClass("NciCactusConn",
    contains=c("BiodbRemotedbConn", "BiodbDownloadable"),

# Public methods {{{2
################################################################################

methods=list(

# Get nb entries {{{3
################################################################################

getNbEntries=function(count=FALSE) {

    n <- NA_integer_

    ids <- .self$getEntryIds()
    if ( ! is.null(ids))
        n <- length(ids)

    return(n)
},

# Get entry page url {{{3
################################################################################

getEntryPageUrl=function(id) {
    return(rep(NA_character_, length(id)))
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
    cch <- .self$getBiodb()$getCache()
    ect <- .self$getPropertyValue('entry.content.type')
    cch$deleteFiles(.self$getCacheId(), subfolder='shortterm', ext=ect)

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
                                  subfolder='shortterm', name=id, ext=ect)
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
    cch <- .self$getBiodb()$getCache()
    ids <- cch$listFiles(.self$getCacheId(), subfolder='shortterm',
                         ext=.self$getPropertyValue('entry.content.type'),
                         extract.name=TRUE)

    return(ids)
}

))
