#' The abstract class for handling file caching.
#'
#' This abstract class is the mother class of concrete classes that manage
#' cache systems for saving downloaded files and request results.
#'
#' It is designed for internal use, but you can still access some of
#' the read-only methods if you wish.
#'
#' @seealso \code{\link{BiodbMain}}, \code{\link{BiodbBiocPersistentCache}},
#' \code{\link{BiodbBiocPersistentCache}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Get a compound CSV file database
#' chebi.tsv <- system.file("extdata", "chebi_extract.tsv", package='biodb')
#'
#' # Get a connector instance:
#' conn <- mybiodb$getFactory()$createConn('comp.csv.file', url=chebi.tsv)
#'
#' # Get all entries
#' entries <- conn$getEntry(conn$getEntryIds())
#'
#' # Get the cache instance:
#' cache <- mybiodb$getPersistentCache()
#'
#' # Get list of files inside the cache:
#' files <- cache$listFiles(conn$getCacheId())
#'
#' # Delete files inside the cache:
#' cache$deleteAllFiles(conn$getCacheId())
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbChildObject.R
#' @export BiodbPersistentCache
#' @exportClass BiodbPersistentCache
BiodbPersistentCache <- methods::setRefClass("BiodbPersistentCache",
    contains='BiodbChildObject',
    fields=list(
        cacheDir='ANY'
    ),
    methods=list(

initialize=function(...) {
    callSuper(...)
    .self$cacheDir <- NULL
},

isReadable=function(conn=NULL) {
    ":\n\nChecks if the cache system is readable.
    \nconn: If not \\code{NULL}, checks if the cache system is readable for
    this particular connector.
    \nReturned value: \\code{TRUE} if the cache system is readable,
    \\code{FALSE} otherwise.
    "

    cfg <- .self$getBiodb()$getConfig()
    
    enabled <- cfg$isEnabled('cache.system')
    enabled <- enabled && ! is.na(.self$getDir())
    if ( ! is.null(conn) && ! conn$isRemotedb())
        enabled <- enabled && cfg$isEnabled('use.cache.for.local.db')
    
    return(enabled)
},

isWritable=function(conn=NULL) {
    ":\n\nChecks if the cache system is writable.
    \nconn: If not \\code{NULL}, checks if the cache system is writable for
    this particular connector.
    \nReturned value: \\code{TRUE} if the cache system is writable,
    \\code{FALSE} otherwise.
    "

    cfg <- .self$getBiodb()$getConfig()
    enabled <- cfg$isEnabled('cache.system')
    enabled <- enabled && ! is.na(.self$getDir())
    enabled <- enabled && ! cfg$get('cache.read.only')
    if ( ! is.null(conn) && ! conn$isRemotedb())
        enabled <- enabled && cfg$isEnabled('use.cache.for.local.db')
    
    return(enabled)
},

getDir=function() {

    if (is.null(.self$cacheDir)) {
        
        # Get configured cache
        .self$cacheDir <- .self$getBiodb()$getConfig()$get('cache.directory')
        
        # Check old default folders
        checkDeprecatedCacheFolders()
        
        # Use default cache if unset
        if (is.null(.self$cacheDir) || is.na(.self$cacheDir)
            || ! is.character(.self$cacheDir))
            .self$cacheDir <- getDefaultCacheDir()

        # Create cache dir if needed
        if ( ! dir.exists(.self$cacheDir))
            dir.create(.self$cacheDir, recursive=TRUE)
    }

    return(.self$cacheDir)
},

getFolderPath=function(cache.id, create=TRUE, fail=FALSE) {
    ":\n\nGets path to the cache system sub-folder dedicated to this cache ID.
    \ncache.id: The cache ID to use.
    \ncreate: If set to TRUE and the folder does not exist, creates it.
    \nfail: If set to TRUE, throws a warning if the folder does not exist.
    \nReturned value: A string containing the path to the folder.
    "

    chk::chk_string(cache.id)
    chk::chk_flag(create)
    chk::chk_flag(fail)

    path <- file.path(.self$getDir(), cache.id)

    if ( ! dir.exists(path)) {
        if (fail) {
            warn('No cache folder "%s" exists for "%s".', path, cache.id)
            if ( ! create)
                path <- NULL
        }
        if (create) {
            logInfo('Create cache folder "%s" for "%s".', path, cache.id)
            dir.create(path, recursive=TRUE)
        }
    }

    return(path)
},

folderExists=function(cache.id) {
    ":\n\nTests if a cache folder exists for this cache ID.
    \ncache.id: The cache ID to use.
    \nReturned value: TRUE if a cache folder exists.
    "

    return(dir.exists(.self$getFolderPath(cache.id, create=FALSE)))
},

getFilePath=function(cache.id, name, ext) {
    ":\n\nGets path of file in cache system.
    \ncache.id: The cache ID to use.
    \nname: A character vector containing file names.
    \next: The extension of the files.
    \nReturned value: A character vector, the same size as \\code{names},
    containing the paths to the files.
    "

    chk::chk_string(cache.id)
    chk::chk_character(name)
    chk::chk_string(ext)

    filepath <- name
    filepath[ ! is.na(name)] <- .self$.doGetFilePath(cache.id,
        name[ ! is.na(name)], ext)

    return(filepath)
},

.doGetFilePath=function(cache.id, name, ext) {
    .self$.abstractMethod()
},

filesExist=function(cache.id) {
    ":\n\nTests if at least one cache file exist for the specified cache ID.
    \ncache.id: The cache ID to use.
    \nReturned value: A single boolean value.
    "
    
    .self$.abstractMethod()
},

fileExist=function(cache.id, name, ext) {
    lifecycle::deprecate_soft('1.1.0', 'fileExist()', "fileExists()")
    return(.self$fileExists(cache.id, name, ext))
},

fileExists=function(cache.id, name, ext) {
    ":\n\nTests if a particular file exist in the cache.
    \ncache.id: The cache ID to use.
    \nname: A character vector containing file names.
    \next: The extension of the files, without the dot (\"html\", \"xml\", etc).
    \nReturned value: A logical vector, the same size as \\code{name}, with
    \\code{TRUE} value if the file exists in the cache, or \\code{FALSE}
    otherwise.
    "

    .self$.abstractMethod()
},

markerExist=function(cache.id, name) {
    ":\n\nTests if markers exist in the cache. Markers are used, for instance,
    by biodb to remember that a downloaded zip file from a database has been
    extracted correctly.
    \ncache.id: The cache ID to use.
    \nname: A character vector containing marker names.
    \nReturned value: A logical vector, the same size as \\code{name}, with
    \\code{TRUE} value if the marker file exists in the cache, or \\code{FALSE}
    otherwise.
    "

    return(.self$fileExists(cache.id=cache.id, name=name, ext='marker'))
},

setMarker=function(cache.id, name) {
    ":\n\nSets a marker.
    \ncache.id: The cache ID to use.
    \nname: A character vector containing marker names.
    \nReturned value: None.
    "

    marker.path <- .self$getFilePath(cache.id, name=name, ext='marker')

    writeChar('', marker.path)
},

getTmpFolderPath=function() {
    ":\n\nGets path to the cache system temporary folder.
    \nReturned value: A string containing the path to the folder.
    "

    tmp_dir <- file.path(.self$getDir(), 'tmp')
    if ( ! dir.exists(tmp_dir))
        dir.create(tmp_dir)

    return(tmp_dir)
},

getUsedCacheIds=function() {
    ":\n\nReturns a list of cache IDs actually used to store cache files.
    \nReturned value: A character vector containing all the cache IDs actually
    used inside the cache system.
    "
    
    ids <- character()

    folders <- Sys.glob(file.path(.self$getDir(), '*-*'))
    ids <- basename(folders)

    return(ids)
},

loadFileContent=function(cache.id, name, ext, output.vector=FALSE) {
    ":\n\nLoads content of files from the cache.
    \ncache.id: The cache ID to use.
    \nname: A character vector containing file names.
    \next: The extension of the files.
    \noutput.vector: If set to \\code{TRUE}, force output to be a \\code{vector}
    instead of a \\code{list}. Where the list contains a \\code{NULL}, the
    \\code{vector} will contain an \\code{NA} value.
    \nReturned value: A list (or a vector if \\code{output.vector} is set to
    \\code{TRUE}), the same size as \\code{name}, containing the contents of the
    files. If some file does not exist, a \\code{NULL} value is inserted inside
    the list.
    "

    if ( ! .self$isReadable())
        error0("Attempt to read from non-readable cache \"",
            .self$getDir(), "\".")

    content <- NULL

    # Read content
    rdCnt <- function(x) {
        if (is.na(x))
            NA_character_
        else if ( ! file.exists(x))
            NULL
        else if (ext == 'RData') {
            load(x)
            c
        } else
            readChar(x, file.info(x)$size, useBytes=TRUE)
    }

    # Read contents from files
    file.paths <- .self$getFilePath(cache.id, name, ext)
    logTrace('Trying to load from cache %s', lst2str(file.paths))
    content <- lapply(file.paths,  rdCnt)
    files.read <- file.paths[ ! vapply(content, is.null, FUN.VALUE=TRUE)]
    logTrace('Loaded from cache %s', lst2str(files.read))

    # Check that the read content is not conflicting with the current locale
    for (i in seq(content)) {
        n <- tryCatch(nchar(content[[i]]), error=function(e) NULL)
        if (is.null(n)) {
            warn0('Error when reading content of file "',
                file.paths[[i]], '". The function `nchar` returned',
                ' an error on the content. The file may be written', 
                ' in a unexpected encoding. Trying latin-1...')
            # The encoding may be wrong, try another one. Maybe LATIN-1
            content[[i]] <- iconv(content[[i]], "iso8859-1")
            n <- tryCatch(nchar(content[[i]]), error=function(e) NULL)
            if (is.null(n))
                error0('Impossible to handle correctly the content of', 
                    ' file "', file.paths[[i]], '". The encoding of', 
                    ' this file is unknown.')
        }
    }

    # Set to NA
    content[content == 'NA' | content == "NA\n"] <- NA_character_

    # Vector ?
    if (output.vector) {
        null_to_na <- function(x) { if (is.null(x)) NA_character_ else x }
        content <- vapply(content, null_to_na, FUN.VALUE='')
    }

    return(content)
},

saveContentToFile=function(content, cache.id, name, ext) {
    ":\n\nSaves content to files into the cache.
    \ncontent: A list or a character vector containing the contents of the
    files. It must have the same length as \\code{name}.
    \ncache.id: The cache ID to use.
    \nname: A character vector containing file names.
    \next: The extension of the files.
    \nReturned value: None.
    "

    .self$.checkWritable(cache.id)

    # Get file paths
    file.paths <- .self$getFilePath(cache.id, name, ext)

    # Check that we have the same number of content and file paths
    if (length(file.paths) != length(content))
        error0('The number of content to save (', length(content),
            ') is different from the number of paths (', length(file.paths),
            ').')

    # Replace NA values with 'NA' string
    content[is.na(content)] <- 'NA'

    # Write content to files
    logTrace('Saving to cache %s', lst2str(file.paths))
    fct <- function(cnt, f) {
        if ( ! is.null(cnt)) {
            if ( ! is.character(cnt))
                cnt <- jsonlite::toJSON(cnt, pretty=TRUE,
                                        digits=NA_integer_)
            # Use cat instead of writeChar, because writeChar was not
            # working with some unicode string (wrong string length).
            cat(cnt, file=f)
        }
    }
    mapply(fct, content, file.paths)
},

.checkWritable=function(cache.id, create=TRUE) {

    if ( ! .self$isWritable())
        error0('Attempt to write into non-writable cache. "',
            .self$getDir(), '".')

    # Make sure the path exists
    if (create)
        path <- .self$getFolderPath(cache.id)
},

moveFilesIntoCache=function(src.file.paths, cache.id, name, ext) {
    ":\n\nMoves exisiting files into the cache.
    \nsrc.file.paths: The current paths of the source files, as a character
    vector.
    \ncache.id: The cache ID to use.
    \nname: A character vector containing file names.
    \next: The extension of the files.
    \nReturned value: None.
    "

    .self$.checkWritable(cache.id)

    chk::chk_character(src.file.paths)
    chk::chk_character(name)
    chk::chk_string(cache.id)
    chk::chk_string(ext)

    # Check that we have the same number of src and file names
    if (length(src.file.paths) != length(name))
        error0('The number of files to move (', length(src.file.paths),
            ') is different from the number of file names (',
            length(name), ').')

    logTrace('Moving files to cache %s.', lst2str(src.file.paths))

    .self$.doMoveFilesToCache(cache.id, src=src.file.paths, name=name, ext=ext)
    logDebug('Done moving files.')

    return(invisible(NULL))
},

.doMoveFilesToCache=function(cache,id, src, name, ext) {
    .self$.abstractMethod()
},

erase=function() {
    ":\n\nErases the whole cache.
    \nReturned value: None.
    "

    # Erase whole cache
    logInfo('Erasing cache "%s".', .self$getDir())
    .self$.doErase()
    unlink(.self$getDir(), recursive=TRUE)
    .self$cacheDir <- NULL

    return(invisible(NULL))
},

.doErase=function() {
    .self$.abstractMethod()
},

deleteFile=function(cache.id, name, ext) {
    ":\n\nDeletes a list of files inside the cache system.
    \ncache.id: The cache ID to use.
    \nname: A character vector containing file names.
    \next: The extension of the files, without the dot (\"html\", \"xml\", etc).
    \nReturned value: None.
    "

    .self$.checkWritable(cache.id, create=FALSE)
    .self$.doDeleteFile(cache.id, name=name, ext=ext)

    return(invisible(NULL))
},

.doDeleteFile=function(cache.id, name, ext) {
    .self$.abstractMethod()
},

deleteAllFiles=function(cache.id, fail=FALSE, prefix=FALSE) {
    ":\n\nDeletes, in the cache system, all files associated with this cache ID.
    \ncache.id: The cache ID to use.
    \nprefix: DEPRECATED If set to TRUE, use cache.id as a prefix, deleting all
    files whose cache.id starts with this prefix.
    \nfail: If set to TRUE, a warning will be emitted if no cache files exist
    for this cache ID.
    \nReturned value: None.
    "

    if ( ! missing(prefix))
        lifecycle::deprecate_stop("1.1.0", "deleteAllFiles(prefix)")
    chk::chk_string(cache.id)
    chk::chk_flag(fail)

    .self$.checkWritable(cache.id, create=FALSE)

    path <- .self$getFolderPath(cache.id, fail=fail, create=FALSE)
    if (.self$folderExists(cache.id)) {
        logInfo('Erasing all files in "%s".', path)
        .self$.doDeleteAllFiles(cache.id)
        unlink(path, recursive=TRUE)
    }

    return(invisible(NULL))
},

.doDeleteAllFiles=function(cache.id) {
    .self$.abstractMethod()
},

deleteFiles=function(cache.id, ext) {
    ":\n\nDeletes all files with the specific extension of the cache ID in the
    cache system.
    \ncache.id: The cache ID to use.
    \next: The extension of the files, without the dot (\"html\", \"xml\", etc).
    Only files having this extension will be deleted.
    \nReturned value: None.
    "

    chk::chk_string(cache.id)
    chk::chk_string(ext)
    
    .self$.checkWritable(cache.id, create=FALSE)

    files <- paste('*', ext, sep='.')
    unlink(file.path(.self$getFolderPath(cache.id), files))
    
    return(invisible(NULL))
},

listFiles=function(cache.id, ext=NULL, extract.name=FALSE,
    full.path=FALSE) {
    ":\n\nLists files present in the cache system.
    \ncache.id: The cache ID to use.
    \next: The extension of the files, without the dot (\"html\", \"xml\", etc).
    \nextract.name: If set to \\code{TRUE}, instead of returning the file paths,
    returns the list of names used to construct the file name:
    [cache_folder]/[cache.id]/[name].[ext].
    \nfull.path: If set to \\code{TRUE}, returns full path for files.
    \nReturned value: The files of found files, or the names of the files if
    \\code{extract.name} is set to \\code{TRUE}.
    "

    chk::chk_string(cache.id)
    chk::chk_null_or(ext, chk::chk_string)
    chk::chk_flag(extract.name)
    chk::chk_flag(full.path)
    if (full.path)
        extract.name <- FALSE

    # Pattern
    pattern <- paste('^.*', sep='')
    if ( ! is.null(ext))
        pattern <- paste(pattern, ext, sep='\\.')
    pattern <- paste(pattern, '$', sep='')

    files <- .self$.doListFiles(cache.id, pattern=pattern, full.path=full.path)

    # Extract only the name part
    if (extract.name) {
        pattern <- paste('^(.*)', sep='')
        if ( ! is.null(ext))
            pattern <- paste(pattern, ext, sep='\\.')
        pattern <- paste(pattern, '$', sep='')
        logDebug0("Extracting accession number from file names associated ",
        "with ", cache.id, " using pattern ", pattern)
        logDebug("files = %s", paste(head(files), collapse=", "))
        files <- sub(pattern, '\\1', files, perl=TRUE)
    }

    return(files)
},

.doListFiles=function(cache.id, pattern, full.path) {
    .self$.abstractMethod()
},

show=function() {
    ":\n\nDisplays information about this object."

    cat("Biodb persistent cache system instance.\n")
    cat("  The used implementation is: ",
        .self$getBiodb()$getConfig()$get('persistent.cache.impl'), ".\n",
        sep='')
    cat("  The path to the cache system is: ", .self$getDir(), ".\n", sep='')
    cat("  The cache is ", (if (.self$isReadable()) "" else "not "),
        "readable.\n", sep='')
    cat("  The cache is ", (if (.self$isWritable()) "" else "not "),
        "writable.\n", sep='')
},

enabled=function() {
    ":\n\nDEPRECATED method. Use now
    \\code{BiodbConfig::isEnabled('cache.system')}.
    "

    lifecycle::deprecate_soft('1.0.0', 'enabled()', "BiodbConfig::isEnabled()")

    return(.self$getBiodb()$getConfig()$isEnabled('cache.system'))
},

enable=function() {
    ":\n\nDEPRECATED method. Use now
    \\code{BiodbConfig::enable('cache.system')}.
    "

    lifecycle::deprecate_soft('1.0.0', 'enable()', "BiodbConfig::enable()")

    .self$getBiodb()$getConfig()$enable('cache.system')
},

disable=function() {
    ":\n\nDEPRECATED method. Use now
    \\code{BiodbConfig::disable('cache.system')}.
    "

    lifecycle::deprecate_soft('1.0.0', 'disable()', "BiodbConfig::disable()")

    .self$getBiodb()$getConfig()$disable('cache.system')
}

))
