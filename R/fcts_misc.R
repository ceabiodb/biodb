getFolder <- function(...) {
    folder <- file.path(...)
    dir.create(folder, showWarnings=FALSE, recursive=TRUE)
    return(folder)
}

getFolderFromVect <- function(path) {
    chk::chk_character(path)
    chk::chk_not_any_na(path)
    return(do.call('getFolder', as.list(path)))
}

convertTolToRange <- function(x, tol, type=c('delta', 'plain', 'ppm')) {
    
    type <- match.arg(type)
    if (type == 'ppm') {
        a <- x - x * tol * 1e-6
        b <- x + x * tol * 1e-6
    } else { 
        a <- x - tol
        b <- x + tol
    }

    return(list(a=a, b=b))
}

extractVersion <- function(filepath) {
    chk::chk_file(filepath)
    
    version <- NULL

    # Read first lines of file
    firstLines <- readLines(filepath, n=5)
    versionLine <- grep('version[ :]', firstLines, value=TRUE)
    if (length(versionLine) > 1)
        versionLine <- versionLine[[1]]

    # Extract version number
    if (length(versionLine) == 1) {
        version <- sub('^.* version:? ([0-9]+\\.[0-9]+(\\.[0-9]+)?)$', '\\1',
            versionLine, perl=TRUE)
        if ( ! chk::vld_match(version, regexp="^[0-9]+\\.[0-9]+(\\.[0-9]+)?$"))
            stop('Impossible to extract version number from line ("',
                versionLine, '") of file "', filepath, '".')
    }

    return(version)
}

splitVersion <- function(version) {
    chk::chk_match(version, regexp="^[0-9]+\\.[0-9]+(\\.[0-9]+)?$")
    
    m <- stringr::str_match(version, "^([0-9]+)\\.([0-9]+)(\\.([0-9]+))?$")
    major <- as.integer(m[1, 2])
    minor <- as.integer(m[1, 3])
    patch <- if (is.na(m[1, 4])) 0L else as.integer(m[1, 5])
    v <- list(major=major, minor=minor, patch=patch)

    return(v)
}

compareVersions <- function(v1, v2) {
    
    cmp <- 0
    v1 <- splitVersion(v1)
    v2 <- splitVersion(v2)
    
    if (v1$major < v2$major)
        cmp <- -1
    else if (v1$major > v2$major)
        cmp <- +1
    else if (v1$minor < v2$minor)
        cmp <- -1
    else if (v1$minor > v2$minor)
        cmp <- +1
    else if (v1$patch < v2$patch)
        cmp <- -1
    else if (v1$patch > v2$patch)
        cmp <- +1
    
    return(cmp)
}

notifyObservers <- function(.obsToNotify, .notifyFct, ...) {

    chk::chk_list(.obsToNotify)
    chk::chk_string(.notifyFct)

    logDebug("Notify observers for %s.", .notifyFct)

    # Notify each observer
    fctCall <- function(o) {
        logDebug("Trying to call function %s on observer %s.", .notifyFct,
            class(o)[[1]])
        logDebug("Functions in observer %s: %s.", class(o)[[1]],
            lst2str(names(o), nCut=0))
        if (.notifyFct %in% names(o))
            o[[.notifyFct]](...)
    }
    lapply(.obsToNotify, fctCall)

    return(invisible(NULL))
}

#' Prepares file contents for saving.
#'
#' @param contents File contents, as a list or a character vector.
#' @return File contents.
prepareFileContents <- function(contents) {

    chk::chkor_vld(chk::vld_character(contents), chk::vld_list(contents))

    # Replace NA values with 'NA' string
    contents[is.na(contents)] <- 'NA'

    # Encode non character contents into JSON
    chars <- vapply(contents, is.character, FUN.VALUE=TRUE)
    contents[ ! chars] <- vapply(contents[ ! chars], jsonlite::toJSON,
        FUN.VALUE='', pretty=TRUE, digits=NA_integer_)

    return(contents)
}

#' Saves contents to files.
#'
#' @param files The file paths to use for saving contents.
#' @param contents The contents to save, as a list or a character vector.
#' @param prepareContents If set to TRUE, then calls prepareFileContents() on
#' the contents before saving them.
#' @return Nothing.
saveContentsToFiles <- function(files, contents, prepareContents=FALSE) {

    chk::chk_character(files)
    chk::chkor_vld(chk::vld_character(contents), chk::vld_list(contents))
    chk::chk_flag(prepareContents)

    if (prepareContents)
        contents <- prepareFileContents(contents)

    # Use cat instead of writeChar, because writeChar is not
    # working with some unicode string (wrong string length).
    mapply(function(cnt, f) cat(cnt, file=f), contents, files)

    return(invisible(NULL))
}

#' Loads the contents of files in memory.
#'
#' This function loads the contents of a list of files and returns the contents
#' as a list, each element being the content of a single file, in the same
#' order. If a file could not be opened, a NULL value is used as the content.
#' NA values are interpreted by default, but this behaviour can be turned off.
#'
#' @param x A character vector containing the paths of the files.
#' @param naValues A character vector listing the content values to convert
#' into NA value. Set to NULL to disable the interpretation of NA values.
#' set to a different set of values to be interpreted.
#' @param outVect If set to TRUE outputs a character vector (converting any
#' NULL value into NA), otherwise outputs a list.
#' @return A list with the contents of the files.
loadFileContents <- function(x, naValues='NA', outVect=FALSE) {

    chk::chk_character(x)
    chk::chk_character(naValues)
    contents <- rep(list(NULL), length(x))

    # Load all contents
    fct <- function(f) {

        content <- NULL
        if (file.exists(f)) {

            # Load content
            ext <- sub('\\.([^.]+)$', '\\1', f, perl=TRUE)
            if (ext == 'RData') {
                load(f) # Content is in variable "c".
                content <- c
            } else
                content <- readChar(f, file.info(f)$size, useBytes=TRUE)

            # Check that the content is not conflicting with the current locale
            n <- tryCatch(nchar(content), error=function(e) NULL)
            if (is.null(n)) {
                warn0('Error when reading content of file "',
                    f, '". The function `nchar` returned',
                    ' an error on the content. The file may be written', 
                    ' in a unexpected encoding. Trying latin-1...')
                # The encoding may be wrong, try another one. Maybe LATIN-1
                content <- iconv(content, "iso8859-1")
                n <- tryCatch(nchar(content), error=function(e) NULL)
                if (is.null(n))
                    error0('Impossible to handle correctly the content of', 
                        ' file "', f, '". The encoding of', 
                        ' this file is unknown.')
            }

            # Set to NA
            if ( ! is.null(naValues) && (content %in% naValues
                || content %in% paste0(naValues, "\n")))
                content <- NA_character_
        }

        return(content)
    } 
    contents <- lapply(x, fct)
    logTrace('Loaded %d files from cache: %s.', length(x), lst2str(x))

    # Vector ?
    if (outVect) {
        null_to_na <- function(x) { if (is.null(x)) NA_character_ else x }
        contents <- vapply(contents, null_to_na, FUN.VALUE='')
    }

    return(contents)
}

#' Declares a class as abstract.
#'
#' Forbids instantiation of an abstract class.
#' This method must be called from within a constructor of an abstract class.
#' It will throw an error if a direct call is made to this constructor.
#'
#' @param cls The name of the abstract class to check.
#' @param obj The object being instantiated.
#' @return Nothing.
#' @export
abstractClass <- function(cls, obj) {

    chk::chk_string(cls)
    chk::chk_not_null(obj)

    if (cls == class(obj)[[1]])
        error('Class %s is abstract and thus cannot be instantiated.', cls)

    return(invisible(NULL))
}

#' Declares a method as abstract
#'
#' This method must be called from within the abstract method.
#'
#' @param obj The object on which the abstract method is called.
#' @return Nothing.
abstractMethod <- function(obj) {

    cls <- class(obj)
    method <- sys.calls()[[length(sys.calls()) - 1]]
    method <- as.character(method)
    method <- method[[1]]
    method <- sub('^[^$]*\\$([^(]*)(\\(.*)?$', '\\1()', method)

    error0("Method ", method, " is not implemented in ", cls, " class.")

    return(invisible(NULL))
}
