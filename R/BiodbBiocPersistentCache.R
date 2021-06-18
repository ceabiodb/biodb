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
        bfc='ANY' # A list of created BiocFileCache objects
    ),
    methods=list(

initialize=function(...) {
    callSuper(...)
    .self$bfc <- list()
},

.getBfc=function(cache.id) {

    if ( ! cache.id %in% names(.self$bfc))
        .self$bfc <- BiocFileCache::BiocFileCache(.self$getFolderPath(cache.id),
            ask=FALSE)

    return(.self$bfc[[cache.id]])
},

.doGetFilePath=function(cache.id, name, ext) {

    bfc <- .self$getBfc(cache.id)
    filename <- paste(name, ext, sep='.')
    return(BiocFileCache::bfcrpath(bfc, filename))
},

filesExist=function(cache.id) {
    # Overwrites super class' method

    bfc <- .self$getBfc(cache.id)
    result <- BiocFileCache::bfcquery(bfc, '.*', file='rname')
    return(BiocFileCache::bfccount(result) > 0)
},

fileExists=function(cache.id, name, ext) {
    # Overwrites super class' method

    bfc <- .self$getBfc(cache.id)
    filenames <- paste(name, ext, sep='.')
    fct <- function(f) {
        result <- BiocFileCache::bfcquery(bfc, f, file='rname', exact=TRUE)
        return(BiocFileCache::bfccount(result) > 0)
    }
    return(vapply(filenames, fct, FUN.VALUE=TRUE))
},

.doMoveFilesToCache=function(cache,id, src, name, ext) {

    bfc <- .self$getBfc(cache.id)
    filenames <- paste(name, ext, sep='.')
    for (i in seq_along(src))
        BiocFileCache::bfcadd(bfc, filenames[[i]], src[[i]], action='move')

    return(invisible(NULL))
},

.doErase=function() {
    .self$bfc <- list()
},

.doDeleteFile=function(cache.id, name, ext) {

    bfc <- .self$getBfc(cache.id)
    filenames <- paste(name, ext, sep='.')
    rids <- character()
    for (filename in filenames) {
        result <- BiocFileCache::bfcquery(bfc, filename, file='rname', exact=TRUE)
        rids <- c(rids, result$rid)
    }
    BiocFileCache::bfcremove(bfc, rids)
},

.doDeleteAllFiles=function(cache.id) {
    if (cache.id %in% names(.self$bfc))
        .self$bfc[[cache.id]] <- NULL
},

.doListFiles=function(cache.id, pattern, full.path) {

    bfc <- .self$getBfc(cache.id)
    result <- BiocFileCache::bfcquery(bfc, pattern, file='rname')
    files <- if (full.path) result$rpath else result$rname

    return(files)
}
))
