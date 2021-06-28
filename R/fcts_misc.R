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
    
    m <- stringr::str_match(version, "([0-9]+).([0-9]+)(.([0-9]+))?")
    
    return(list(major=m[1, 2], minor=m[1, 3],
        patch=(if (is.na(m[1, 4])) 0 else m[1, 5]) ))
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
        logDebug("Functions in observer %s: %s.", class(o), lst2str(names(o),
            nCut=0))
        if (.notifyFct %in% names(o))
            o[[.notifyFct]](...)
    }
    lapply(.obsToNotify, fctCall)

    return(invisible(NULL))
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
