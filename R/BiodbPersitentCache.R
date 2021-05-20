#' A class for handling file caching.
#'
#' This class manages a cache system for saving downloaded files and request
#' results.
#'
#' It is designed for internal use, but you can still access some of
#' the read-only methods if you wish.
#'
#' Inside the cache folder, one folder is created for each cache ID (each remote
#' database has one single cache ID, always the same ,except if you change its
#' URL). In each of theses folders are stored the cache files for this database.
#'
#' @seealso \code{\link{BiodbMain}}.
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
    methods=list(

getVersion=function() {
    ":\n\nReturns the cache version.
    \nReturned value: The current cache version.
    "

    version <- '0.1'

    # Check version file
    version_file <- file.path(.self$getDir(), 'VERSION')
    if (file.exists(version_file))
        version <- readLines(version_file)

    return(version)
},

getDir=function() {
    ":\n\nGets the absolute path to the cache directory.
    \nReturned value: The absolute path of the cache directory.
    "

    cachedir <- .self$getBiodb()$getConfig()$get('cache.directory')

    # Check old cache folder
    env <- Sys.getenv()
    if ('HOME' %in% names(env)) {
        old_cachedir <- file.path(env[['HOME']], '.biodb.cache')
        if ( ! is.null(cachedir) && ! is.na(cachedir)
            && old_cachedir != cachedir && file.exists(old_cachedir)) {
            if (file.exists(cachedir))
                warn0('An old cache folder ("', old_cachedir,
                    '") is still present on this machine, ',
                    'but you are now using the new cache folder "',
                    cachedir, '". Please, consider removing the old ',
                    'location since it has no utility anymore.')
            else {
                # Move folder to new location
                dir.create(dirname(cachedir), recursive=TRUE)
                file.rename(old_cachedir, cachedir)
                logInfo0('Cache folder location has been moved from "',
                    old_cachedir, '" to "', cachedir, '".')
            }
        }
    }

    # Create cache dir if needed
    if ( ! is.na(cachedir) && ! file.exists(cachedir))
        dir.create(cachedir, recursive=TRUE)

    return(cachedir)
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

filesExist=function(cache.id) {
    ":\n\nTests if at least one cache file exist for the specified cache ID.
    \ncache.id: The cache ID to use.
    \nReturned value: A single boolean value.
    "
    
    return(length(Sys.glob(file.path(.self$getFolderPath(cache.id), '*'))) > 0)
},

fileExist=function(cache.id, name, ext) {
    ":\n\nTests if a particular file exist in the cache.
    \ncache.id: The cache ID to use.
    \nname: A character vector containing file names.
    \next: The extension of the files, without the dot (\"html\", \"xml\", etc).
    \nReturned value: A logical vector, the same size as \\code{name}, with
    \\code{TRUE} value if the file exists in the cache, or \\code{FALSE}
    otherwise.
    "

    return(file.exists(.self$getFilePath(cache.id, name, ext)))
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

    return(.self$fileExist(cache.id=cache.id, name=name, ext='marker'))
},

setMarker=function(cache.id, name) {
    ":\n\nSets a marker.
    \ncache.id: The cache ID to use.
    \nname: A character vector containing marker names.
    \nReturned value: None.
    "

    # Make sure the path exists
    path <- file.path(.self$getDir(), cache.id)
    if ( ! dir.exists(path))
        dir.create(path, recursive=TRUE)

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

getFolderPath=function(cache.id) {
    ":\n\nGets path to the cache system sub-folder dedicated to this cache ID.
    \ncache.id: The cache ID to use.
    \nReturned value: A string containing the path to the folder.
    "

    return(file.path(.self$getDir(), cache.id))
},

getFilePath=function(cache.id, name, ext) {
    ":\n\nGets path of file in cache system.
    \ncache.id: The cache ID to use.
    \nname: A character vector containing file names.
    \next: The extension of the files.
    \nReturned value: A character vector, the same size as \\code{names},
    containing the paths to the files.
    "

    # Replace unwanted characters
    name <- gsub('[^A-Za-z0-9._-]', '_', name)

    # Set file path
    filepaths <- file.path(.self$getFolderPath(cache.id),
        paste(name, '.', ext, sep=''))

    # Set NA values
    filepaths[is.na(name)] <- NA_character_

    return(filepaths)
},

getUsedCacheIds=function() {
    ":\n\nReturns a list of cache IDs actually used to store cache files.
    \nReturned value: A character vector containing all the cache IDs actually
    used inside the cache system.
    "
    
    ids <- character()

    folders <- Sys.glob(file.path(.self$getDir(), '*'))
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

.checkWritable=function(cache.id) {

    if ( ! .self$isWritable())
        error0('Attempt to write into non-writable cache. "',
            .self$getDir(), '".')

    # Make sure the path exists
    path <- file.path(.self$getDir(), cache.id)
    if ( ! dir.exists(path))
        dir.create(path, recursive=TRUE)
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

    # Get destination file paths
    dstFilePaths <- .self$getFilePath(cache.id, name, ext)

    # Check that we have the same number of src and dst file paths
    if (length(src.file.paths) != length(dstFilePaths))
        error0('The number of files to move (', length(src.file.paths),
            ') is different from the number of destination paths (',
            length(dstFilePaths), ').')

    # Move files
    logTrace('Moving files to cache ', lst2str(src.file.paths))
    logTrace('Destination files are ', lst2str(dstFilePaths))
    file.rename(src.file.paths, dstFilePaths)
    logDebug('Done moving files.')
},

erase=function() {
    ":\n\nErases the whole cache.
    \nReturned value: None.
    "

    # Erase whole cache
    logInfo('Erasing cache "%s".', .self$getDir())
    unlink(.self$getDir(), recursive=TRUE)

    invisible(NULL)
},

deleteFile=function(cache.id, name, ext) {
    ":\n\nDeletes a list of files inside the cache system.
    \ncache.id: The cache ID to use.
    \nname: A character vector containing file names.
    \next: The extension of the files, without the dot (\"html\", \"xml\", etc).
    \nReturned value: None.
    "

    if ( ! .self$isWritable())
        error0('Attempt to write into non-writable cache. "',
            .self$getDir(), '".')

    # Get file paths
    file.paths <- .self$getFilePath(cache.id, name, ext)

    # Delete files
    lapply(file.paths, unlink)

    invisible(NULL)
},

deleteAllFiles=function(cache.id, fail=FALSE, prefix=FALSE) {
    ":\n\nDeletes, in the cache system, all files associated with this cache ID.
    \ncache.id: The cache ID to use.
    \nprefix: If set to TRUE, use cache.id as a prefix, deleting all files
    whose cache.id starts with this prefix.
    \nfail: If set to TRUE, a warning will be emitted if no cache files exist
    for this cache ID.
    \nReturned value: None.
    "

    paths <- character()

    # Set paths
    if (prefix) {
        paths <- Sys.glob(file.path(.self$getDir(), paste0(cache.id, '*')))
    }
    else {
        path <- file.path(.self$getDir(), cache.id)
        if ( ! file.exists(path)) {
            msg <- paste0('No cache files exist for ', cache.id, '.')
            if (fail) warn(msg) else logInfo(msg)
        }
        else
            paths <- path
    }

    # Erase cache files
    for (path in paths) {
        logInfo('Erasing all files in "%s".', path)
        unlink(path, recursive=TRUE)
    }

    invisible(NULL)
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
    
    if ( ! .self$isWritable())
        error0('Attempt to write into non-writable cache. "',
            .self$getDir(), '".')

    files <- paste('*', ext, sep='.')

    unlink(file.path(.self$getDir(), cache.id, files))
    
    invisible(NULL)
},

listFiles=function(cache.id, ext=NA_character_, extract.name=FALSE,
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

    # Pattern
    pattern <- paste('^.*', sep='')
    if ( ! is.na(ext))
        pattern <- paste(pattern, ext, sep='\\.')
    pattern <- paste(pattern, '$', sep='')

    # List files
    path <- .self$getFolderPath(cache.id=cache.id)
    logDebug("List files in %s using pattern %s.", path, pattern)
    files <- list.files(path=path, pattern=pattern)

    # Extract only the name part
    if (extract.name) {
        pattern <- paste('^(.*)', sep='')
        if ( ! is.na(ext))
            pattern <- paste(pattern, ext, sep='\\.')
        pattern <- paste(pattern, '$', sep='')
        logDebug0("Extracting accession number from file names in ", path,
        " using pattern ", pattern)
        logDebug("files = %s", paste(head(files), collapse=", "))
        files <- sub(pattern, '\\1', files, perl=TRUE)
        
    # Set full path
    } else if (full.path) {
        files <- file.path(path, files)
    }

    return(files)
},

show=function() {
    ":\n\nDisplays information about this object."

    cat("Biodb persistent cache system instance.\n")
    cat("  The path to the cache system is: ", .self$getDir(), "\n", sep='')
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
