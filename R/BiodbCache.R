# vi: fdm=marker ts=4 et cc=80

# BiodbCache {{{1
################################################################################

#' A class for handling file caching.
#'
#' This class manages a cache system for saving downloaded files and request
#' results. It is designed for internal use, but you can still access some of
#' the read-only methods if you wish.
#'
#' @param content       A \code{character vector} containing contents to save.
#' @param cache.id      An ID to use in the cache.
#' @param ext           The extension of the file, without the dot: 'html',
#'                      'xml', etc.
#' @param extract.name  Instead of returning the file paths, returns the list of
#'                      names used to construct the file name:
#'                      [cache_folder]/[subfolder]/[connid]-[name].[ext].
#' @param name          The name of the file or the marker. Vector of
#'                      characters. Length can be greater than one.
#' @param output.vector Force output to be a \code{vector} instead of a
#'                      \code{list}. Where the list contains a \code{NULL}, the
#'                      \code{vector} will contain a \code{NA} value.
#' @param subfolder     The subfolder inside the cache system. Supported values
#'                      are: 'shortterm' and 'longterm'. The 'shortterm' folder
#'                      contains individual entry files. The 'longterm' folder
#'                      contains zip files of whole databases.
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

# Initialize {{{3
################################################################################

initialize=function(...) {
    callSuper(...)
},

# Get directory {{{3
################################################################################

getDir=function() {
    "Get the absolute path to the cache directory."

    cachedir <- .self$getBiodb()$getConfig()$get('cache.directory')

    # Create cache dir if needed
    if ( ! is.na(cachedir) && ! file.exists(cachedir))
        dir.create(cachedir)

    return(cachedir)
},

# Is readable {{{3
################################################################################

isReadable=function() {
    "Returns TRUE if the cache system is readable."

    return( .self$getBiodb()$getConfig()$isEnabled('cache.system') && ! is.na(.self$getDir()))
},

# Is writable {{{3
################################################################################

isWritable=function() {
    "Returns TRUE if the cache system is writable."

    return( .self$getBiodb()$getConfig()$isEnabled('cache.system') && ! is.na(.self$getDir()) && ! .self$getBiodb()$getConfig()$get('cache.read.only'))
},

# File exists {{{3
################################################################################

fileExist=function(cache.id, subfolder, name, ext) {
    "Test if files exist in the cache."

    exists <- file.exists(.self$getFilePath(cache.id, subfolder, name, ext))

    return(exists)
},

# Marker exists {{{3
################################################################################

markerExist=function(cache.id, subfolder, name) {
    "Test if markers exist in the cache. Markers are used, for instance, by biodb to remember that a downloaded zip file from a database has been extracted correctly."

    return(.self$fileExist(cache.id=cache.id, subfolder=subfolder, name=name, ext='marker'))
},

# Set marker {{{3
################################################################################

setMarker=function(cache.id, subfolder, name) {
    "Set a marker."

    marker.path <- .self$getFilePath(cache.id=cache.id, subfolder=subfolder, name=name, ext='marker')

    writeChar('', marker.path)
},

# Get file path {{{3
################################################################################

getFilePath=function(cache.id, subfolder, name, ext) {
    "Get path of file in cache system."

    # Replace unwanted characters
    name <- gsub('[^A-Za-z0-9._-]', '_', name)

    # Set file path
    filepaths <- file.path(.self$getSubFolderPath(subfolder), paste(cache.id, '-', name, '.', ext, sep=''))

    # Set NA values
    filepaths[is.na(name)] <- NA_character_

    return(filepaths)
},

# Load file content {{{3
################################################################################

loadFileContent=function(cache.id, subfolder, name, ext, output.vector=FALSE) {
    "Load content of files from the cache."

    if ( ! .self$isReadable())
        .self$message('error', paste("Attempt to read from non-readable cache \"", .self$getDir(), "\".", sep=''))

    content <- NULL

    # Read contents from files
    file.paths <- .self$getFilePath(cache.id, subfolder, name, ext)
    .self$message('debug', paste("Trying to load from cache \"", paste(file.paths[seq_len(min(10, length(file.paths)))], collapse=", "), if (length(file.paths) > 10) " ..." else "", "\".", sep=''))
    content <- lapply(file.paths, function(x) { if (is.na(x)) NA_character_ else if ( ! file.exists(x)) NULL else if (ext == 'RData') { load(x) ; c} else readChar(x, file.info(x)$size, useBytes=TRUE)} )
    files.read <- file.paths[ ! vapply(content, is.null, FUN.VALUE=TRUE)]
    if (length(files.read) == 0)
        .self$message('debug', "No files loaded from cache.")
    else
        .self$message('debug', paste("Loaded from cache \"", paste(if (length(files.read) > 10) c(files.read[seq_len(10)], '...') else files.read, collapse=", ") ,"\".", sep=''))

    # Check that the read content is not conflicting with the current locale
    for (i in seq(content)) {
        n <- tryCatch(nchar(content[[i]]), error=function(e) NULL)
        if (is.null(n)) {
            .self$message('caution', paste("Error when reading content of file \"", file.paths[[i]], "\". The function `nchar` returned an error on the content. The file may be written in a unexpected encoding. Trying latin-1...", sep=''))
            # The encoding may be wrong, try another one. Maybe LATIN-1
            content[[i]] <- iconv(content[[i]], "iso8859-1")
            n <- tryCatch(nchar(content[[i]]), error=function(e) NULL)
            if (is.null(n))
                .self$message('error', paste("Impossible to handle correctly the content of file \"", file.paths[[i]], "\". The encoding of this file is unknown.", sep=''))
        }
    }

    # Set to NA
    content[content == 'NA' | content == "NA\n"] <- NA_character_

    # Vector ?
    if (output.vector)
        content <- vapply(content, function(x) if (is.null(x)) NA_character_ else x, FUN.VALUE='')

    return(content)
},

# Save content into file {{{3
################################################################################

saveContentToFile=function(content, cache.id, subfolder, name, ext) {
    "Save content to files into the cache."

    if ( ! .self$isWritable())
        .self$message('error', paste("Attempt to write into non-writable cache. \"", .self$getDir(), "\".", sep=''))

    # Get file paths
    file.paths <- .self$getFilePath(cache.id, subfolder, name, ext)

    # Check that we have the same number of content and file paths
    if (length(file.paths) != length(content))
        .self$message('error', paste("The number of content to save (", length(content), ") is different from the number of paths (", length(file.paths), ").", sep=''))

    # Replace NA values with 'NA' string
    if (ext != 'RData')
        content[is.na(content)] <- 'NA'

    # Write content to files
    .self$message('debug', paste("Saving to cache \"", paste(if (length(file.paths) > 10) c(file.paths[seq_len(10)], '...') else file.paths, collapse=", ") ,"\".", sep=''))
    if (ext == 'RData')
        mapply(function(c, f) { save(c, file=f) }, content, file.paths)
    else
        mapply(function(c, f) { if ( ! is.null(c)) cat(c, file=f) }, content, file.paths) # Use cat instead of writeChar, because writeChar was not working with some unicode string (wrong string length).
},

# Get subfolder path {{{3
################################################################################

getSubFolderPath=function(subfolder) {
    "Get the absolute path of a subfolder inside the cache system."

    folder.path <- .self$.getSubfolderPath(subfolder)

    # Create folder if needed
    if ( ! is.na(folder.path) && ! file.exists(folder.path))
        dir.create(folder.path)

    return(folder.path)
},

# Erase folder {{{3
################################################################################

eraseFolder=function(subfolder=NA_character_) {

    # Erase whole cache
    if (is.na(subfolder) || ! .self$getBiodb()$getConfig()$isEnabled('cache.subfolders'))
        folder.to.erase <- .self$getDir()

    # Erase subfolder
    else
        folder.to.erase <- .self$.getSubfolderPath(subfolder)

    # Erase
    .self$message('info', paste("Erasing cache folder ", folder.to.erase, ".", sep=''))
    unlink(folder.to.erase, recursive=TRUE)
},

# Delete file {{{3
################################################################################

deleteFile=function(cache.id, subfolder, name, ext) {
    "Delete one file inside the cache system."

    if ( ! .self$isWritable())
        .self$message('error', paste("Attempt to write into non-writable cache. \"", .self$getDir(), "\".", sep=''))

    # Get file paths
    file.paths <- .self$getFilePath(cache.id, subfolder, name, ext)

    # Delete files
    lapply(file.paths, unlink)
},

# Delete files {{{3
################################################################################

deleteFiles=function(cache.id, subfolder, ext=NA_character_) {
    "Delete files inside the cache system."

    if ( ! .self$isWritable())
        .self$message('error', paste("Attempt to write into non-writable cache. \"", .self$getDir(), "\".", sep=''))

    files <- paste(cache.id, '*',sep='-')
    if ( ! is.na(ext))
        files <- paste(files, ext, sep='.')

    unlink(file.path(.self$getSubFolderPath(subfolder), files))
},

# List files {{{3
################################################################################

listFiles=function(cache.id, subfolder, ext=NA_character_, extract.name=FALSE) {
    "List files present in the cache system."

    # Pattern
    pattern <- paste('^', cache.id, '-.*', sep='')
    if ( ! is.na(ext))
        pattern <- paste(pattern, ext, sep='\\.')
    pattern <- paste(pattern, '$', sep='')

    # List files
    dir <- .self$getSubFolderPath(subfolder)
    .self$message('debug', paste("List files in", dir, "using pattern ", pattern))
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
    cat("Biodb cache system instance.\n")
    cat("  The cache is ", (if (.self$isReadable()) "" else "not "), "readable.\n", sep='')
    cat("  The cache is ", (if (.self$isWritable()) "" else "not "), "writable.\n", sep='')
},

# Private methods {{{2
################################################################################

# Get subfolder path {{{3
################################################################################

.getSubfolderPath=function(subfolder) {

    cfg.subfolder.key <- paste(subfolder, 'cache', 'subfolder', sep='.')

    # Check subfolder
    if ( ! .self$getBiodb()$getConfig()$isDefined(cfg.subfolder.key))
        .self$message('error', paste("Unknown cache folder \"", folder, "\".", sep=''))

    # Get subfolder path
    if (.self$getBiodb()$getConfig()$isEnabled('cache.subfolders'))
        folder.path <- file.path(.self$getDir(), .self$getBiodb()$getConfig()$get(cfg.subfolder.key))
    else
        folder.path <- .self$getDir()
},

# Deprecated methods {{{2
################################################################################

# Enabled {{{3
################################################################################

enabled=function() {

    .self$.deprecatedMethod("BiodbConfig::isEnabled('cache.system')")

    return(.self$getBiodb()$getConfig()$isEnabled('cache.system'))
},

# Enable {{{3
################################################################################

enable=function() {

    .self$.deprecatedMethod("BiodbConfig::enable('cache.system')")
    
    .self$getBiodb()$getConfig()$enable('cache.system')
},

# Disable {{{3
################################################################################

disable=function() {

    .self$.deprecatedMethod("BiodbConfig::disable('cache.system')")
    
    .self$getBiodb()$getConfig()$disable('cache.system')
}

))
