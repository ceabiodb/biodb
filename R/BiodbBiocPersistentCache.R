#' A persistent cache implementation that uses BiocCache package.
#'
#'
#' @seealso \code{\link{BiodbPersistentCache}},
#' \code{\link{BiodbBiocPersistentCache}}.
#'
#' @include BiodbPersistentCache.R
#' @import BiocFileCache
#' @export
BiodbBiocPersistentCache <- R6::R6Class('BiodbBiocPersistentCache',
inherit=BiodbPersistentCache,

public=list(

initialize=function(...) {
    super$initialize(...)
    private$cacheId2Bfc <- list()
}
),

private=list(
    cacheId2Bfc=NULL
,
getBfc=function(cache.id, create=FALSE) {

    bfc <- NULL

    # Already exists
    if (cache.id %in% names(private$cacheId2Bfc))
        bfc <-private$cacheId2Bfc[[cache.id]]

    # Instantiate
    else if (create || self$folderExists(cache.id)) {

        cachedFiles <- NULL

        # Get cache folder
        folder <- self$getFolderPath(cache.id)

        # Is this an upgrade?
        upgrade <- ! file.exists(file.path(folder, 'BiocFileCache.sqlite'))
        if (upgrade)
            cachedFiles <- Sys.glob(file.path(folder, '*'))

        # Create/Instantiate bfc
        bfc <- BiocFileCache::BiocFileCache(folder, ask=FALSE)

        # Integrate existing files
        if ( ! is.null(cachedFiles) && length(cachedFiles) > 0) {
            msg <- sprintf('Converting cache folder of %s into BiocFileCache',
                cache.id)
            prg <- Progress$new(biodb=self$getBiodb(), msg=msg,
                total=length(cachedFiles))
            for (f in cachedFiles) {
                BiocFileCache::bfcadd(bfc, basename(f), f, action='move')
                prg$increment()
            }
        }

        private$cacheId2Bfc[[cache.id]] <- bfc
    }

    return(bfc)
},

doGetFilePath=function(cache.id, name, ext) {

    file.paths <- character()

    if (length(name) > 0) {
        bfc <- private$getBfc(cache.id, create=TRUE)
        filename <- paste(name, ext, sep='.')
        file.paths <- vapply(filename, function(f)
            BiocFileCache::bfcrpath(bfc, rnames=f), FUN.VALUE='')
    }

    return(file.paths)
},

doFilesExist=function(cache.id) {

    fExist <- FALSE
    bfc <- private$getBfc(cache.id)
    if ( ! is.null(bfc)) {
        result <- BiocFileCache::bfcquery(bfc, '.*', field='rname')
        fExist <- BiocFileCache::bfccount(result) > 0
    }

    return(fExist)
},

doFileExists=function(cache.id, name, ext) {

    fExists <- NULL

    bfc <- private$getBfc(cache.id)
    if (is.null(bfc)) {
        fExists <- rep(FALSE, length(name))
    } else {
        filenames <- paste(name, ext, sep='.')
        fct <- function(f) {
            if (is.na(f) || f == '')
                return(FALSE)
            result <- BiocFileCache::bfcquery(bfc, f, field='rname', exact=TRUE)
            return(BiocFileCache::bfccount(result) > 0)
        }
        fExists <- vapply(filenames, fct, FUN.VALUE=TRUE)
    }

    return(fExists)
},

doMoveFilesToCache=function(cache,id, src, name, ext) {

    bfc <- private$getBfc(cache.id, create=TRUE)
    filenames <- paste(name, ext, sep='.')
    for (i in seq_along(src))
        BiocFileCache::bfcadd(bfc, filenames[[i]], src[[i]], action='move')

    return(invisible(NULL))
},

doErase=function() {
    private$cacheId2Bfc <- list()

    return(invisible(NULL))
},

doDeleteFile=function(cache.id, name, ext) {

    bfc <- private$getBfc(cache.id)
    if ( ! is.null(bfc)) {
        filenames <- paste(name, ext, sep='.')
        rids <- character()
        for (filename in filenames) {
            result <- BiocFileCache::bfcquery(bfc, filename, field='rname', exact=TRUE)
            rids <- c(rids, result$rid)
        }
        BiocFileCache::bfcremove(bfc, rids)
    }

    return(invisible(NULL))
},

doDeleteAllFiles=function(cache.id) {

    if (cache.id %in% names(private$cacheId2Bfc))
        private$cacheId2Bfc[[cache.id]] <- NULL

    return(invisible(NULL))
},

doListFiles=function(cache.id, pattern, full.path) {

    bfc <- private$getBfc(cache.id)
    if (is.null(bfc)) {
        files <- character()
    } else {
        result <- BiocFileCache::bfcquery(bfc, pattern, field='rname')
        files <- if (full.path) result$rpath else result$rname
    }

    return(files)
},

doSaveContentToFile=function(cache.id, content, name, ext) {

    bfc <- private$getBfc(cache.id)
    if ( ! is.null(bfc)) {
    
        existsInCache <- self$fileExists(cache.id, name, ext)

        # Overwrite contents of existing files
        if (any(existsInCache)) {
            file.paths <- self$getFilePath(cache.id, name[existsInCache], ext)
            logTrace('Overwriting contents of cache files %s',
                lst2str(file.paths))
            # Use cat instead of writeChar, because writeChar is not
            # working with some unicode string (wrong string length).
            mapply(function(cnt, f) cat(cnt, file=f), content[existsInCache],
                file.paths)
        }

        # Add non-existing files
        tmpDir <- self$getTmpFolderPath()
        fct <- function(cnt, name) {
            tmpfile <- tempfile(name, fileext=paste0('.', ext), tmpdir=tmpDir)
            # Use cat instead of writeChar, because writeChar is not
            # working with some unicode string (wrong string length).
            cat(cnt, file=tmpfile)
            BiocFileCache::bfcadd(bfc, paste(name, ext, sep='.'), tmpfile,
                action='move')
        }
        mapply(fct, content[ ! existsInCache], name[ ! existsInCache])
    }

    return(invisible(NULL))
}
))
