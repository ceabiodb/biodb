# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbCache {{{1
################################################################################

# Declaration {{{2
################################################################################

#' A class for handling file caching.
#'
#' This class manages a cache system for saving downloaded files and request
#' results.
#'
#' It is designed for internal use, but you can still access some of
#' the read-only methods if you wish.
#'
#' Inside the cache folder, two subfolders are created: "shortterm" and
#' "longterm". The "shortterm" folder contains individual entry files. The
#' "longterm" folder contains zip files of whole databases.
#'
#' @seealso \code{\link{Biodb}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get a connector instance:
#' conn <- mybiodb$getFactory()$createConn('chebi')
#'
#' # Get the cache instance:
#' cache <- mybiodb$getCache()
#'
#' # Get list of files inside the cache:
#' files <- cache$listFiles(conn$getCacheId(), 'shortterm')
#'
#' # Delete files inside the cache:
#' cache$deleteFiles(conn$getCacheId(), 'shortterm')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbChildObject.R
#' @export BiodbCache
#' @exportClass BiodbCache
BiodbCache <- methods::setRefClass("BiodbCache",
    contains='BiodbChildObject',
    methods=list(

# Public methods {{{2
################################################################################

# Get directory {{{3
################################################################################

getDir=function() {
    ":\n\nGets the absolute path to the cache directory.
    \nReturned value: The absolute path of the cache directory.
    "

    cachedir <- .self$getBiodb()$getConfig()$get('cache.directory')

    # Create cache dir if needed
    if ( ! is.na(cachedir) && ! file.exists(cachedir))
        dir.create(cachedir)

    return(cachedir)
},

# Is readable {{{3
################################################################################

isReadable=function() {
    ":\n\nChecks if the cache system is readable.
    \nReturned value: \\code{TRUE} if the cache system is readable,
    \\code{FALSE} otherwise.
    "

    cfg <- .self$getBiodb()$getConfig()
    return(cfg$isEnabled('cache.system') && ! is.na(.self$getDir()))
},

# Is writable {{{3
################################################################################

isWritable=function() {
    ":\n\nChecks if the cache system is writable.
    \nReturned value: \\code{TRUE} if the cache system is writable,
    \\code{FALSE} otherwise.
    "

    cfg <- .self$getBiodb()$getConfig()
    return(cfg$isEnabled('cache.system') && ! is.na(.self$getDir())
           && ! cfg$get('cache.read.only'))
},

# File exists {{{3
################################################################################

fileExist=function(cache.id, subfolder, name, ext) {
    ":\n\nTests if files exist in the cache.
    \ncache.id: The cache ID to use.
    \nsubfolder: A subfolder to use (\"longterm\" or \"shortterm\").
    \nname: A character vector containing file names.
    \next: The extension of the files, without the dot (\"html\", \"xml\", etc).
    \nReturned value: A logical vector, the same size as \\code{name}, with
    \\code{TRUE} value if the file exists in the cache, or \\code{FALSE}
    otherwise.
    "

    exists <- file.exists(.self$getFilePath(cache.id, subfolder, name, ext))

    return(exists)
},

# Marker exists {{{3
################################################################################

markerExist=function(cache.id, subfolder, name) {
    ":\n\nTests if markers exist in the cache. Markers are used, for instance, by
    biodb to remember that a downloaded zip file from a database has been
    extracted correctly.
    \ncache.id: The cache ID to use.
    \nsubfolder: A subfolder to use (\"longterm\" or \"shortterm\").
    \nname: A character vector containing marker names.
    \nReturned value: A logical vector, the same size as \\code{name}, with
    \\code{TRUE} value if the marker file exists in the cache, or \\code{FALSE}
    otherwise.
    "

    b <- .self$fileExist(cache.id=cache.id, subfolder=subfolder, name=name,
                         ext='marker')

    return(b)
},

# Set marker {{{3
################################################################################

setMarker=function(cache.id, subfolder, name) {
    ":\n\nSets a marker.
    \ncache.id: The cache ID to use.
    \nsubfolder: A subfolder to use (\"longterm\" or \"shortterm\").
    \nname: A character vector containing marker names.
    \nReturned value: None.
    "

    marker.path <- .self$getFilePath(cache.id=cache.id, subfolder=subfolder,
                                     name=name, ext='marker')

    writeChar('', marker.path)
},

# Get file path {{{3
################################################################################

getFilePath=function(cache.id, subfolder, name, ext) {
    ":\n\nGets path of file in cache system.
    \ncache.id: The cache ID to use.
    \nsubfolder: A subfolder to use (\"longterm\" or \"shortterm\").
    \nname: A character vector containing file names.
    \next: The extension of the files.
    \nReturned value: A character vector, the same size as \\code{names},
    containing the paths to the files.
    "

    # Replace unwanted characters
    name <- gsub('[^A-Za-z0-9._-]', '_', name)

    # Set file path
    filepaths <- file.path(.self$getSubFolderPath(subfolder),
                           paste(cache.id, '-', name, '.', ext, sep=''))

    # Set NA values
    filepaths[is.na(name)] <- NA_character_

    return(filepaths)
},

# Load file content {{{3
################################################################################

loadFileContent=function(cache.id, subfolder, name, ext, output.vector=FALSE) {
    ":\n\nLoads content of files from the cache.
    \ncache.id: The cache ID to use.
    \nsubfolder: A subfolder to use (\"longterm\" or \"shortterm\").
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
        .self$error("Attempt to read from non-readable cache \"",
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
    file.paths <- .self$getFilePath(cache.id, subfolder, name, ext)
    .self$debug2List('Trying to load from cache', file.paths)
    content <- lapply(file.paths,  rdCnt)
    files.read <- file.paths[ ! vapply(content, is.null, FUN.VALUE=TRUE)]
    .self$debug2List('Loaded from cache', files.read)

    # Check that the read content is not conflicting with the current locale
    for (i in seq(content)) {
        n <- tryCatch(nchar(content[[i]]), error=function(e) NULL)
        if (is.null(n)) {
            .self$caution('Error when reading content of file "',
                          file.paths[[i]], '". The function `nchar` returned',
                          ' an error on the content. The file may be written', 
                          ' in a unexpected encoding. Trying latin-1...')
            # The encoding may be wrong, try another one. Maybe LATIN-1
            content[[i]] <- iconv(content[[i]], "iso8859-1")
            n <- tryCatch(nchar(content[[i]]), error=function(e) NULL)
            if (is.null(n))
                .self$error('Impossible to handle correctly the content of', 
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

# Save content into file {{{3
################################################################################

saveContentToFile=function(content, cache.id, subfolder, name, ext) {
    ":\n\nSaves content to files into the cache.
    \ncontent: A list or a character vector containing the contents of the
    files. It must have the same length as \\code{name}.
    \ncache.id: The cache ID to use.
    \nsubfolder: A subfolder to use (\"longterm\" or \"shortterm\").
    \nname: A character vector containing file names.
    \next: The extension of the files.
    \nReturned value: None.
    "

    if ( ! .self$isWritable())
        .self$error('Attempt to write into non-writable cache. "',
                    .self$getDir(), '".')

    # Get file paths
    file.paths <- .self$getFilePath(cache.id, subfolder, name, ext)

    # Check that we have the same number of content and file paths
    if (length(file.paths) != length(content))
        .self$error('The number of content to save (', length(content),
                    ') is different from the number of paths (',
                    length(file.paths), ').')

    # Replace NA values with 'NA' string
    if (ext != 'RData')
        content[is.na(content)] <- 'NA'

    # Write content to files
    .self$debug2List('Saving to cache', file.paths)
    if (ext == 'RData')
        mapply(function(c, f) { save(c, file=f) }, content, file.paths)
    else
        # Use cat instead of writeChar, because writeChar was not working with
        # some unicode string (wrong string length).
        mapply(function(c, f) { if ( ! is.null(c)) cat(c, file=f) },
               content, file.paths)
},

# Get subfolder path {{{3
################################################################################

getSubFolderPath=function(subfolder) {
    ":\n\nGets the absolute path of a subfolder inside the cache system.
    \nsubfolder: A subfolder to use (\"longterm\" or \"shortterm\").
    \nReturned value: The absolute path to the subfolder.
    "

    folder.path <- .self$.getSubfolderPath(subfolder)

    # Create folder if needed
    if ( ! is.na(folder.path) && ! file.exists(folder.path))
        dir.create(folder.path)

    return(folder.path)
},

# Erase folder {{{3
################################################################################

eraseFolder=function(subfolder=NA_character_) {
    ":\n\nErases the cache.
    \nsubfolder: The subfolder to erase (\"longterm\" or \"shortterm\"). If
    unset, the whole cache will be erased.
    \nReturned value: None.
    "

    # Erase whole cache
    cfg <- .self$getBiodb()$getConfig()
    if (is.na(subfolder) || ! cfg$isEnabled('cache.subfolders'))
        folder.to.erase <- .self$getDir()

    # Erase subfolder
    else
        folder.to.erase <- .self$.getSubfolderPath(subfolder)

    # Erase
    .self$info("Erasing cache folder ", folder.to.erase, ".")
    unlink(folder.to.erase, recursive=TRUE)
},

# Delete file {{{3
################################################################################

deleteFile=function(cache.id, subfolder, name, ext) {
    ":\n\nDeletes a list of files inside a subfolder of the cache system.
    \ncache.id: The cache ID to use.
    \nsubfolder: A subfolder to use (\"longterm\" or \"shortterm\").
    \nname: A character vector containing file names.
    \next: The extension of the files, without the dot (\"html\", \"xml\", etc).
    \nReturned value: None.
    "

    if ( ! .self$isWritable())
        .self$error('Attempt to write into non-writable cache. "',
                    .self$getDir(), '".')

    # Get file paths
    file.paths <- .self$getFilePath(cache.id, subfolder, name, ext)

    # Delete files
    lapply(file.paths, unlink)
},

# Delete files {{{3
################################################################################

deleteFiles=function(cache.id, subfolder, ext=NA_character_) {
    ":\n\nDeletes all files inside a subfolder of the cache system.
    \ncache.id: The cache ID to use.
    \nsubfolder: A subfolder to use (\"longterm\" or \"shortterm\").
    \next: The extension of the files, without the dot (\"html\", \"xml\", etc).
    Only files having this extension will be deleted.
    \nReturned value: None.
    "

    if ( ! .self$isWritable())
        .self$error('Attempt to write into non-writable cache. "',
                    .self$getDir(), '".')

    files <- paste(cache.id, '*',sep='-')
    if ( ! is.na(ext))
        files <- paste(files, ext, sep='.')

    unlink(file.path(.self$getSubFolderPath(subfolder), files))
},

# List files {{{3
################################################################################

listFiles=function(cache.id, subfolder, ext=NA_character_, extract.name=FALSE) {
    ":\n\nLists files present in the cache system.
    \ncache.id: The cache ID to use.
    \nsubfolder: A subfolder to use (\"longterm\" or \"shortterm\").
    \next: The extension of the files, without the dot (\"html\", \"xml\", etc).
    \nextract.name: If set to \\code{TRUE}, instead of returning the file paths,
    returns the list of names used to construct the file name:
    [cache_folder]/[subfolder]/[connid]-[name].[ext].
    \nReturned value: The files of found files, or the names of the files if
    \\code{extract.name} is set to \\code{TRUE}.
    "

    # Pattern
    pattern <- paste('^', cache.id, '-.*', sep='')
    if ( ! is.na(ext))
        pattern <- paste(pattern, ext, sep='\\.')
    pattern <- paste(pattern, '$', sep='')

    # List files
    dir <- .self$getSubFolderPath(subfolder)
    .self$debug("List files in", dir, "using pattern ", pattern)
    files <- list.files(path=dir, pattern=pattern)

    # Extract only the name part
    if (extract.name) {
        pattern <- paste('^', cache.id, '-(.*)', sep='')
        if ( ! is.na(ext))
            pattern <- paste(pattern, ext, sep='\\.')
        pattern <- paste(pattern, '$', sep='')
        files <- sub(pattern, '\\1', files, perl=TRUE)
    }

    return(files)
},

# Show {{{3
################################################################################

show=function() {
    ":\n\nDisplays information about this object."

    cat("Biodb cache system instance.\n")
    cat("  The cache is ", (if (.self$isReadable()) "" else "not "),
        "readable.\n", sep='')
    cat("  The cache is ", (if (.self$isWritable()) "" else "not "),
        "writable.\n", sep='')
},

# Private methods {{{2
################################################################################

# Get subfolder path {{{3
################################################################################

.getSubfolderPath=function(subfolder) {

    cfg <- .self$getBiodb()$getConfig()
    cfg.subfolder.key <- paste(subfolder, 'cache', 'subfolder', sep='.')

    # Check subfolder
    if ( ! cfg$isDefined(cfg.subfolder.key))
        .self$error('Unknown cache folder "', folder, '".')

    # Get subfolder path
    if (cfg$isEnabled('cache.subfolders'))
        folder.path <- file.path(.self$getDir(), cfg$get(cfg.subfolder.key))
    else
        folder.path <- .self$getDir()
},

# Deprecated methods {{{2
################################################################################

# Enabled {{{3
################################################################################

enabled=function() {
    ":\n\nDEPRECATED method. Use now
    \\code{BiodbConfig::isEnabled('cache.system')}.
    "

    .self$.deprecatedMethod("BiodbConfig::isEnabled('cache.system')")

    return(.self$getBiodb()$getConfig()$isEnabled('cache.system'))
},

# Enable {{{3
################################################################################

enable=function() {
    ":\n\nDEPRECATED method. Use now
    \\code{BiodbConfig::enable('cache.system')}.
    "

    .self$.deprecatedMethod("BiodbConfig::enable('cache.system')")

    .self$getBiodb()$getConfig()$enable('cache.system')
},

# Disable {{{3
################################################################################

disable=function() {
    ":\n\nDEPRECATED method. Use now
    \\code{BiodbConfig::disable('cache.system')}.
    "

    .self$.deprecatedMethod("BiodbConfig::disable('cache.system')")

    .self$getBiodb()$getConfig()$disable('cache.system')
}

))
