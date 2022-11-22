.CACHE_ID_PATTERN <- '^.+-[0-9a-f]{16,}$'

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
#' @export
BiodbPersistentCache <- R6::R6Class("BiodbPersistentCache",

public=list(

#' @description
#' New instance initializer. Persistent cache objects must not be created
#' directly.
#' Instead, access the cache instance through the BiodbMain instance using the
#' getPersistentCache() method.
#' @param cfg An instance of the BiodbConfig class.
#' @param bdb An instance of the BiodbMain class.
#' @return Nothing.
initialize=function(cfg, bdb=NULL) {

    chk::chk_is(cfg, 'BiodbConfig')
    private$bdb <- bdb
    private$cfg <- cfg
    private$cacheDir <- NULL

    return(invisible(NULL))
},

#' @description
#' Checks if the cache system is readable.
#' @param conn If not \\code{NULL}, checks if the cache system is readable for
#'     this particular connector.
#' @return \\code{TRUE} if the cache system is readable,
#'     \\code{FALSE} otherwise.
isReadable=function(conn=NULL) {

    enabled <- private$cfg$isEnabled('cache.system')
    enabled <- enabled && ! is.na(self$getDir())
    if ( ! is.null(conn) && ! conn$isRemotedb())
        enabled <- enabled && private$cfg$isEnabled('use.cache.for.local.db')
    
    return(enabled)
},

#' @description
#' Checks if the cache system is writable.
#' @param conn If not \\code{NULL}, checks if the cache system is writable for
#'     this particular connector.
#' @return \\code{TRUE} if the cache system is writable,
#'     \\code{FALSE} otherwise.
isWritable=function(conn=NULL) {

    enabled <- private$cfg$isEnabled('cache.system')
    enabled <- enabled && ! is.na(self$getDir())
    enabled <- enabled && ! private$cfg$get('cache.read.only')
    if ( ! is.null(conn) && ! conn$isRemotedb())
        enabled <- enabled && private$cfg$isEnabled('use.cache.for.local.db')
    
    return(enabled)
},

#' @description
#' Gets the path to the persistent cache folder.
#' @return The path to the cache folder as a character value.
getDir=function() {

    if (is.null(private$cacheDir)) {
        
        # Get configured cache
        private$cacheDir <- private$cfg$get('cache.directory')
        
        # Check old default folders
        checkDeprecatedCacheFolders()
        
        # Use default cache if unset
        if (is.null(private$cacheDir) || is.na(private$cacheDir)
            || ! is.character(private$cacheDir))
            private$cacheDir <- getDefaultCacheDir()

        # Create cache dir if needed
        if ( ! dir.exists(private$cacheDir))
            dir.create(private$cacheDir, recursive=TRUE)
    }

    return(private$cacheDir)
},

#' @description
#' Gets path to the cache system sub-folder dedicated to this cache ID.
#' @param cache.id The cache ID to use.
#' @param create If set to TRUE and the folder does not exist, creates it.
#' @param fail If set to TRUE, throws a warning if the folder does not exist.
#' @return A string containing the path to the folder.
getFolderPath=function(cache.id, create=TRUE, fail=FALSE) {

    chk::chk_string(cache.id)
    chk::chk_match(cache.id, .CACHE_ID_PATTERN)
    chk::chk_flag(create)
    chk::chk_flag(fail)

    path <- file.path(self$getDir(), cache.id)

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

#' @description
#' Tests if a cache folder exists for this cache ID.
#' @param cache.id The cache ID to use.
#' @return TRUE if a cache folder exists.
folderExists=function(cache.id) {

    chk::chk_string(cache.id)
    chk::chk_match(cache.id, .CACHE_ID_PATTERN)

    return(dir.exists(self$getFolderPath(cache.id, create=FALSE)))
},

#' @description
#' Gets path of file in cache system.
#' @param cache.id The cache ID to use.
#' @param name A character vector containing file names.
#' @param ext The extension of the files.
#' @return A character vector, the same size as \\code{names},
#'     containing the paths to the files.
getFilePath=function(cache.id, name, ext) {

    chk::chk_string(cache.id)
    chk::chk_match(cache.id, .CACHE_ID_PATTERN)
    chk::chk_character(name)
    chk::chk_string(ext)

    filepath <- name
    is.valid.name <- ( ! is.na(name)) & (name != '')
    filepath[is.valid.name] <- private$doGetFilePath(cache.id,
        name[is.valid.name], ext)

    return(filepath)
},

#' @description
#' Tests if at least one cache file exist for the specified cache ID.
#' @param cache.id The cache ID to use.
#' @return A single boolean value.
filesExist=function(cache.id) {
    
    chk::chk_string(cache.id)
    chk::chk_match(cache.id, .CACHE_ID_PATTERN)

    return(private$doFilesExist(cache.id))
},

#' @description
#' DEPRECATED. Use fileExists().
#' @param cache.id The cache ID to use.
#' @param name A character vector containing file names.
#' @param ext The extension of the files, without the dot (\"html\", \"xml\",
#' etc).
#' @return A logical vector, the same size as \\code{name}, with \\code{TRUE}
#' value if the file exists in the cache, or \\code{FALSE} otherwise.
fileExist=function(cache.id, name, ext) {
    lifecycle::deprecate_soft('1.1.0', 'fileExist()', "fileExists()")
    return(self$fileExists(cache.id, name, ext))
},

#' @description
#' Tests if a particular file exist in the cache.
#' @param cache.id The cache ID to use.
#' @param name A character vector containing file names.
#' @param ext The extension of the files, without the dot (\"html\", \"xml\",
#' etc).
#' @return A logical vector, the same size as \\code{name}, with \\code{TRUE}
#' value if the file exists in the cache, or \\code{FALSE} otherwise.
fileExists=function(cache.id, name, ext) {

    chk::chk_string(cache.id)
    chk::chk_match(cache.id, .CACHE_ID_PATTERN)
    chk::chk_character(name)
    chk::chk_string(ext)

    return(private$doFileExists(cache.id, name, ext))
},

#' @description
#' DEPRECATED. Use markerExists().
#' @param cache.id The cache ID to use.
#' @param name A character vector containing marker names.
#' @return A logical vector, the same size as \\code{name}, with
#' \\code{TRUE} value if the marker file exists in the cache, or \\code{FALSE}
#' otherwise.
markerExist=function(cache.id, name) {
    lifecycle::deprecate_soft('1.1.0', 'markerExist()', "markerExists()")
    return(self$markerExists(cache.id=cache.id, name=name))
},

#' @description
#' Tests if markers exist in the cache. Markers are used, for instance,
#' by biodb to remember that a downloaded zip file from a database has been
#' extracted correctly.
#' @param cache.id The cache ID to use.
#' @param name A character vector containing marker names.
#' @return A logical vector, the same size as \\code{name}, with
#' \\code{TRUE} value if the marker file exists in the cache, or \\code{FALSE}
#' otherwise.
markerExists=function(cache.id, name) {
    return(self$fileExists(cache.id=cache.id, name=name, ext='marker'))
},

#' @description
#' Sets a marker.
#' @param cache.id The cache ID to use.
#' @param name A character vector containing marker names.
#' @return Nothing.
setMarker=function(cache.id, name) {

    if ( ! self$markerExists(cache.id=cache.id, name=name)) {
        ext <- 'marker'
        tmpDir <- self$getTmpFolderPath()
        emptyFile <- tempfile("marker", tmpdir=tmpDir, fileext=ext)
        writeChar('', emptyFile)
        self$addFilesToCache(emptyFile, cache.id=cache.id, name=name, ext=ext,
            action='move')
    }

    return(invisible(NULL))
},

#' @description
#' Gets path to the cache system temporary folder.
#' @return A string containing the path to the folder.
getTmpFolderPath=function() {

    # This temporary folder located inside the cache folder is needed in order
    # to be able to move/rename files into the right cache location.
    # When creating files in the system temporary folder, which may reside on a
    # different partition, moving a file could fail as in the following error:
    #     cannot rename file '/tmp/Rtmpld18y7/10182e3a086e7b8a7.tsv' to
    #     '/home/pr228844/dev/biodb/cache/comp.csv.file-58e...c4/2e3...a7.tsv',
    #     reason 'Invalid cross-device link'

    tmp_dir <- file.path(self$getDir(), 'tmp')
    if ( ! dir.exists(tmp_dir))
        dir.create(tmp_dir)

    return(tmp_dir)
},

#' @description
#' Returns a list of cache IDs actually used to store cache files.
#' @return A character vector containing all the cache IDs actually
#'     used inside the cache system.
getUsedCacheIds=function() {
    
    ids <- character()

    folders <- Sys.glob(file.path(self$getDir(), '*-*'))
    ids <- basename(folders)

    return(ids)
},

#' @description
#' Loads content of files from the cache.
#' @param cache.id The cache ID to use.
#' @param name A character vector containing file names.
#' @param ext The extension of the files.
#' @param output.vector If set to \\code{TRUE}, force output to be a
#' \\code{vector} instead of a \\code{list}. Where the list contains a
#' \\code{NULL}, the \\code{vector} will contain an \\code{NA} value.
#' @return A list (or a vector if \\code{output.vector} is set to
#' \\code{TRUE}), the same size as \\code{name}, containing the contents of the
#' files. If some file does not exist, a \\code{NULL} value is inserted inside
#' the list.
loadFileContent=function(cache.id, name, ext, output.vector=FALSE) {

    chk::chk_string(cache.id)
    chk::chk_match(cache.id, .CACHE_ID_PATTERN)
    chk::chk_character(name)
    chk::chk_string(ext)
    chk::chk_flag(output.vector)

    if ( ! self$isReadable())
        error0("Attempt to read from non-readable cache \"",
            self$getDir(), "\".")

    logTrace('Trying to load %d files from cache: %s.', length(name),
        lst2str(name))
    content <- rep(list(NULL), length(name))
    file.exist <- self$fileExists(cache.id, name, ext)
    logTrace('file.exist = %s.', lst2str(file.exist))
    content[is.na(name)] <- NA_character_
    content[name == ""] <- NA_character_
    file.paths <- self$getFilePath(cache.id, name[file.exist], ext)
    content[file.exist] <- loadFileContents(file.paths)

    # Convert list to vector
    if (output.vector) {
        content[vapply(content, is.null, FUN.VALUE=TRUE)] <- NA_character_
        content <- unlist(content)
    }

    return(content)
},

#' @description
#' Saves content to files into the cache.
#' @param content A list or a character vector containing the contents of the
#' files. It must have the same length as \\code{name}.
#' @param cache.id The cache ID to use.
#' @param name A character vector containing file names.
#' @param ext The extension of the files.
#' @return Nothing.
saveContentToFile=function(content, cache.id, name, ext) {

    chk::chkor_vld(chk::vld_character(content), chk::vld_list(content))
    chk::chk_string(cache.id)
    chk::chk_match(cache.id, .CACHE_ID_PATTERN)
    chk::chk_character(name)
    chk::chk_string(ext)

    private$checkWritable(cache.id)

    # Check that we have the same number of content and names
    if (length(name) != length(content))
        error0('The number of content to save (', length(content),
            ') is different from the number of names (', length(name),
            ').')

    # Remove elements with NULL content
    nulls <- vapply(content, is.null, FUN.VALUE=TRUE)
    content <- content[ ! nulls]
    name <- name[ ! nulls]

    content <- prepareFileContents(content)

    # Save content
    private$doSaveContentToFile(cache.id, content=content, name=name, ext=ext)

    return(invisible(NULL))
},

#' @description
#' Adds exisiting files into the cache.
#' @param src.file.paths The current paths of the source files, as a character
#' vector.
#' @param cache.id The cache ID to use.
#' @param name A character vector containing file names.
#' @param ext The extension of the files.
#' @param action Specifies if files have to be moved or copied into the cache.
#' @return Nothing.
addFilesToCache=function(src.file.paths, cache.id, name, ext,
    action=c('copy', 'move')) {

    chk::chk_character(src.file.paths)
    chk::chk_character(name)
    chk::chk_string(cache.id)
    chk::chk_match(cache.id, .CACHE_ID_PATTERN)
    chk::chk_string(ext)
    action <- match.arg(action)

    private$checkWritable(cache.id)

    # Check that we have the same number of src and file names
    if (length(src.file.paths) != length(name))
        error0('The number of files to move (', length(src.file.paths),
            ') is different from the number of file names (',
            length(name), ').')

    logTrace('Moving files to cache %s.', lst2str(src.file.paths))

    private$doAddFilesToCache(cache.id, src=src.file.paths, name=name, ext=ext,
        action=action)
    logDebug('Done adding files.')

    return(invisible(NULL))
},

#' @description
#' Copies exisiting files into the cache.
#' @param src.file.paths The current paths of the source files, as a character
#' vector.
#' @param cache.id The cache ID to use.
#' @param name A character vector containing file names.
#' @param ext The extension of the files.
#' @return Nothing.
copyFilesIntoCache=function(src.file.paths, cache.id, name, ext) {

    self$addFilesToCache(src.file.paths, cache.id=cache.id, name=name,
        ext=ext, action='copy')

    return(invisible(NULL))
},

#' @description
#' Moves exisiting files into the cache.
#' @param src.file.paths The current paths of the source files, as a character
#' vector.
#' @param cache.id The cache ID to use.
#' @param name A character vector containing file names.
#' @param ext The extension of the files.
#' @return Nothing.
moveFilesIntoCache=function(src.file.paths, cache.id, name, ext) {

    self$addFilesToCache(src.file.paths, cache.id=cache.id, name=name,
        ext=ext, action='move')

    return(invisible(NULL))
},

#' @description
#' Erases the whole cache.
#' @return Nothing.
erase=function() {

    # Erase whole cache
    logInfo('Erasing cache "%s".', self$getDir())
    private$doErase()
    unlink(self$getDir(), recursive=TRUE)
    private$cacheDir <- NULL

    return(invisible(NULL))
},

#' @description
#' Deletes a list of files inside the cache system.
#' @param cache.id The cache ID to use.
#' @param name A character vector containing file names.
#' @param ext The extension of the files, without the dot (\"html\", \"xml\",
#' etc).
#' @return Nothing.
deleteFile=function(cache.id, name, ext) {

    chk::chk_string(cache.id)
    chk::chk_match(cache.id, .CACHE_ID_PATTERN)
    chk::chk_character(name)
    chk::chk_string(ext)

    private$checkWritable(cache.id, create=FALSE)
    private$doDeleteFile(cache.id, name=name, ext=ext)

    return(invisible(NULL))
},

#' @description
#' Deletes, in the cache system, all files associated with this cache ID.
#' @param cache.id The cache ID to use.
#' @param prefix DEPRECATED If set to TRUE, use cache.id as a prefix, deleting
#' all files whose cache.id starts with this prefix.
#' @param fail If set to TRUE, a warning will be emitted if no cache files
#' exist for this cache ID.
#' @return Nothing.
deleteAllFiles=function(cache.id, fail=FALSE, prefix=FALSE) {

    if ( ! missing(prefix))
        lifecycle::deprecate_stop("1.1.0", "deleteAllFiles(prefix)")
    chk::chk_string(cache.id)
    chk::chk_match(cache.id, .CACHE_ID_PATTERN)
    chk::chk_flag(fail)

    private$checkWritable(cache.id, create=FALSE)

    path <- self$getFolderPath(cache.id, fail=fail, create=FALSE)
    if (self$folderExists(cache.id)) {
        logInfo('Erasing all files in "%s".', path)
        private$doDeleteAllFiles(cache.id)
        unlink(path, recursive=TRUE)
    }

    return(invisible(NULL))
},

#' @description
#' Deletes all files with the specific extension of the cache ID in the
#' cache system.
#' @param cache.id The cache ID to use.
#' @param ext The extension of the files, without the dot (\"html\", \"xml\",
#' etc).  Only files having this extension will be deleted.
#' @return Nothing.
deleteFiles=function(cache.id, ext) {

    chk::chk_string(cache.id)
    chk::chk_match(cache.id, .CACHE_ID_PATTERN)
    chk::chk_string(ext)
    
    private$checkWritable(cache.id, create=FALSE)

    files <- paste('*', ext, sep='.')
    unlink(file.path(self$getFolderPath(cache.id), files))
    
    return(invisible(NULL))
},

#' @description
#' Lists files present in the cache system.
#' @param cache.id The cache ID to use.
#' @param ext The extension of the files, without the dot (\"html\", \"xml\",
#' etc).
#' @param extract.name If set to \\code{TRUE}, instead of returning the file
#' paths, returns the list of names used to construct the file name:
#' \[cache_folder\]/\[cache.id\]/\[name\].\[ext\].
#' @param full.path If set to \\code{TRUE}, returns full path for files.
#' @return The files of found files, or the names of the files if
#' \\code{extract.name} is set to \\code{TRUE}.
listFiles=function(cache.id, ext=NULL, extract.name=FALSE, full.path=FALSE) {

    chk::chk_string(cache.id)
    chk::chk_match(cache.id, .CACHE_ID_PATTERN)
    chk::chk_null_or(ext, vld=chk::vld_string)
    chk::chk_flag(extract.name)
    chk::chk_flag(full.path)
    if (full.path)
        extract.name <- FALSE

    # Pattern
    pattern <- paste('^.*', sep='')
    if ( ! is.null(ext))
        pattern <- paste(pattern, ext, sep='\\.')
    pattern <- paste(pattern, '$', sep='')

    files <- private$doListFiles(cache.id, pattern=pattern, full.path=full.path)

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

#' @description
#' Displays information about this object.
print=function() {
    cat("Biodb persistent cache system instance.\n")
    cat("  The used implementation is: ",
        private$cfg$get('persistent.cache.impl'), ".\n",
        sep='')
    cat("  The path to the cache system is: ", self$getDir(), ".\n", sep='')
    cat("  The cache is ", (if (self$isReadable()) "" else "not "),
        "readable.\n", sep='')
    cat("  The cache is ", (if (self$isWritable()) "" else "not "),
        "writable.\n", sep='')
},

#' @description
#' DEPRECATED method. Use now
#' \\code{BiodbConfig :isEnabled('cache.system')}.
enabled=function() {

    lifecycle::deprecate_soft('1.0.0', 'enabled()', "BiodbConfig::isEnabled()")

    return(private$cfg$isEnabled('cache.system'))
},

#' @description
#' DEPRECATED method. Use now
#' \\code{BiodbConfig :enable('cache.system')}.
enable=function() {

    lifecycle::deprecate_soft('1.0.0', 'enable()', "BiodbConfig::enable()")

    private$cfg$enable('cache.system')
},

#' @description
#' DEPRECATED method. Use now
#' \\code{BiodbConfig :disable('cache.system')}.
disable=function() {

    lifecycle::deprecate_soft('1.0.0', 'disable()', "BiodbConfig::disable()")

    private$cfg$disable('cache.system')
}
),

private=list(
    cacheDir=NULL,
    cfg=NULL,
    bdb=NULL,

doGetFilePath=function(cache.id, name, ext) {
    abstractMethod(self)
},

doFilesExist=function(cache.id) {
    abstractMethod(self)
},

doFileExists=function(cache.id, name, ext) {
    abstractMethod(self)
},

checkWritable=function(cache.id, create=TRUE) {

    if ( ! self$isWritable())
        error0('Attempt to write into non-writable cache. "',
            self$getDir(), '".')

    # Make sure the path exists
    if (create)
        path <- self$getFolderPath(cache.id)

    return(invisible(NULL))
},

doAddFilesToCache=function(cache.id, src, name, ext, action) {
    abstractMethod(self)
},

doErase=function() {
    abstractMethod(self)
},

doDeleteFile=function(cache.id, name, ext) {
    abstractMethod(self)
},

doDeleteAllFiles=function(cache.id) {
    abstractMethod(self)
},

doListFiles=function(cache.id, pattern, full.path) {
    abstractMethod(self)
},

doSaveContentToFile=function(cache.id, content, name, ext) {
    abstractMethod(self)
}
))
