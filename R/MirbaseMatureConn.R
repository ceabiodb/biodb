# vi: fdm=marker ts=4 et cc=80 tw=80

# MirbaseMatureConn {{{1
################################################################################

# Declaration {{{2
################################################################################

#' Mirbase Mature connector class.
#'
#' This is the connector class for Mirbase Mature database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('mirbase.mature')
#'
#' # Get an entry
#' e <- conn$getEntry('MIMAT0000433')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include MirbaseConn.R
#' @include BiodbDownloadable.R
#' @export MirbaseMatureConn
#' @exportClass MirbaseMatureConn
MirbaseMatureConn <- methods::setRefClass("MirbaseMatureConn",
    contains=c("MirbaseConn", "BiodbDownloadable"),

# Public methods {{{2
################################################################################

methods=list(

# Get entry page url {{{3
################################################################################

getEntryPageUrl=function(id) {
    # Overrides super class' method.

    url <- c(.self$getPropValSlot('urls', 'base.url'), 'cgi-bin', 'mature.pl')
    v <- vapply(id,
                function(x) BiodbUrl(url=url,
                                     params=list(mature_acc=x))$toString(),
                FUN.VALUE='')

    return(v)
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

    # Download
    .self$download()

    # Load content from cache
    cch <- .self$getBiodb()$getCache()
    ext <- .self$getPropertyValue('entry.content.type')
    content <- cch$loadFileContent(.self$getCacheId(),
                                   name=entry.id, ext=ext, output.vector=TRUE)

    return(content)
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
        cch$deleteFiles(.self$getCacheId(),
                        ext=.self$getPropertyValue('entry.content.type'))
        cch$saveContentToFile(contents, cache.id=.self$getCacheId(),
                              name=ids,
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
    ids <- cch$listFiles(.self$getCacheId(),
                         ext=.self$getPropertyValue('entry.content.type'),
                         extract.name=TRUE)

    # Filter out wrong IDs
    ids <- ids[grepl("^MIMAT[0-9]+$", ids, perl=TRUE)]

    return(ids)
}

))
