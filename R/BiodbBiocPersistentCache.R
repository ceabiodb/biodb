#' A persistent cache implementation that uses BiocCache package.
#'
#'
#' @seealso \code{\link{BiodbPersistentCache}},
#' \code{\link{BiodbBiocPersistentCache}}.
#'
#' @include BiodbPersistentCache.R
#' @export BiodbBiocPersistentCache
#' @exportClass BiodbBiocPersistentCache
BiodbBiocPersistentCache <- methods::setRefClass('BiodbBiocPersistentCache',
    contains='BiodbPersistentCache',
    fields=list(
        cacheId2Bfc='ANY' # A list of created BiocFileCache objects
    ),
    methods=list(

initialize=function(...) {
    callSuper(...)
    .self$cacheId2Bfc <- list()
},

.getBfc=function(cache.id, create=FALSE) {

    bfc <- NULL

    # Already exists
    if (cache.id %in% names(.self$cacheId2Bfc))
        bfc <-.self$cacheId2Bfc[[cache.id]]

    # Instantiate
    else if (create || .self$folderExists(cache.id)) {

        cachedFiles <- NULL

        # Get cache folder
        folder <- .self$getFolderPath(cache.id)

        # Is this an upgrade?
        upgrade <- ! file.exists(file.path(folder, 'BiocFileCache.sqlite'))
        if (upgrade)
            cachedFiles <- Sys.glob(file.path(folder, '*'))

        # Create/Instantiate bfc
        .self$cacheId2Bfc <- BiocFileCache::BiocFileCache(folder, ask=FALSE)

        # Integrate existing files
        if ( ! is.null(cachedFiles) && length(cachedFiles) > 0) {
            msg <- sprintf('Converting cache folder of %s into BiocFileCache',
                cache.id)
            prg <- Progress$new(biodb=.self$getBiodb(), msg=msg,
                total=length(cachedFiles))
            for (f in cachedFiles) {
                BiocFileCache::bfcadd(bfc, basename(f), f, action='move')
                prg$increment()
            }
        }

        bfc <-.self$cacheId2Bfc[[cache.id]]
    }

    return(bfc)
},

.doGetFilePath=function(cache.id, name, ext) {

    bfc <- .self$getBfc(cache.id, create=TRUE)
    filename <- paste(name, ext, sep='.')
    return(BiocFileCache::bfcrpath(bfc, filename))
},

.doFilesExist=function(cache.id) {

    fExist <- FALSE
    bfc <- .self$getBfc(cache.id)
    if ( ! is.null(bfc)) {
        result <- BiocFileCache::bfcquery(bfc, '.*', file='rname')
        fExist <- BiocFileCache::bfccount(result) > 0
    }

    return(fExist)
},

.doFileExists=function(cache.id, name, ext) {

    fExists <- NULL

    bfc <- .self$getBfc(cache.id)
    if (is.null(bfc)) {
        fExists <- rep(FALSE, length(name))
    } else {
        filenames <- paste(name, ext, sep='.')
        fct <- function(f) {
            result <- BiocFileCache::bfcquery(bfc, f, file='rname', exact=TRUE)
            return(BiocFileCache::bfccount(result) > 0)
        }
        fExists <- vapply(filenames, fct, FUN.VALUE=TRUE)
    }

    return(fExists)
},

.doMoveFilesToCache=function(cache,id, src, name, ext) {

    bfc <- .self$getBfc(cache.id, create=TRUE)
    filenames <- paste(name, ext, sep='.')
    for (i in seq_along(src))
        BiocFileCache::bfcadd(bfc, filenames[[i]], src[[i]], action='move')

    return(invisible(NULL))
},

.doErase=function() {
    .self$cacheId2Bfc <- list()

    return(invisible(NULL))
},

.doDeleteFile=function(cache.id, name, ext) {

    bfc <- .self$getBfc(cache.id)
    if ( ! is.null(bfc)) {
        filenames <- paste(name, ext, sep='.')
        rids <- character()
        for (filename in filenames) {
            result <- BiocFileCache::bfcquery(bfc, filename, file='rname', exact=TRUE)
            rids <- c(rids, result$rid)
        }
        BiocFileCache::bfcremove(bfc, rids)
    }

    return(invisible(NULL))
},

.doDeleteAllFiles=function(cache.id) {

    if (cache.id %in% names(.self$cacheId2Bfc))
        .self$cacheId2Bfc[[cache.id]] <- NULL

    return(invisible(NULL))
},

.doListFiles=function(cache.id, pattern, full.path) {

    bfc <- .self$getBfc(cache.id)
    if (is.null(bfc)) {
        result <- BiocFileCache::bfcquery(bfc, pattern, file='rname')
        files <- if (full.path) result$rpath else result$rname
    } else {
        files <- character()
    }

    return(files)
}
))
