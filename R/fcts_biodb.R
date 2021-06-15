#' Create a new BiodbMain instance.
#'
#' Instantiates a new BiodbMain object by calling the constructor.
#'
#' @seealso \code{\link{BiodbMain}}.
#'
#' @param ... The parameters to pass to the BiodbMain constructor. See
#' \code{\link{BiodbMain}}.
#' @return A new BiodbMain instance.
#' @examples
#' # Create a new BiodbMain instance:
#' mybiodb <- biodb::newInst()
#'
#' # Terminate the instance:
#' mybiodb$terminate()
#' @export
newInst <- function(...) {
    return(BiodbMain$new(...))
}

#' Get connector class name.
#'
#' Gets the name of the connector class corresponding to a connector.
#'
#' @param connName A connector name (e.g.: "mass.csv.file").
#' @return The name of the corresponding connector class (e.g.:
#' "MassCsvFileConn").
#'
#' @examples
#' biodb::getConnClassName('foo.db')
#'
#' @export
getConnClassName <- function(connName) {
    return(paste0(connNameToClassPrefix(connName), 'Conn'))
}

#' Get entry class name.
#'
#' Gets the name of the entry class corresponding to a connector.
#'
#' @param connName A connector name (e.g.: "mass.csv.file").
#' @return The name of the corresponding entry class (e.g.:
#' "MassCsvFileEntry").
#'
#' @examples
#' biodb::getEntryClassName('foo.db')
#'
#' @export
getEntryClassName <- function(connName) {
    return(paste0(connNameToClassPrefix(connName), 'Entry'))
}

#' Get connector types.
#'
#' Get the list of available connector types.
#'
#' @return A character vector containing the connector types.
#'
#' @examples
#' biodb::getConnTypes()
#'
#' @export
getConnTypes <- function() {
    return(c('plain', 'compound', 'mass'))
}

#' Get entry types.
#'
#' Get the list of available entry types.
#'
#' @return A character vector containing the entry types.
#'
#' @examples
#' biodb::getEntryTypes()
#'
#' @export
getEntryTypes <- function() {
    return(c('plain', 'csv', 'html', 'json', 'list', 'sdf', 'txt', 'xml'))
}

#' Convert connector name into class prefix.
#'
#' Converts the connector name into the class prefix (e.g.: "mass.csv.file" -->
#' "MassCsvFile").
#'
#' @param connName A connector name (e.g.: "mass.csv.file").
#' @return The corresponding class prefix (e.g.: "MassCsvFile").
#' @import chk
connNameToClassPrefix <- function(connName) {
    chk::chk_string(connName)

    s <- connName
    indices <- as.integer(gregexpr('\\.[a-z]', s, perl=TRUE)[[1]])
    indices <- indices + 1  # We are interested in the letter after the dot.
    indices <- c(1, indices) # Add first letter.
    for (i in indices)
        s <- paste(substring(s, 1, i - 1), toupper(substring(s, i, i)),
            substring(s, i + 1), sep='')
    s <- gsub('.', '', s, fixed=TRUE) # Remove dots

    return(s)
}

#' Get the main package logger.
#'
#' Gets the main package logger, parent of all loggers of this package.
#'
#' @return The main package logger (named "biodb") as a lgr::Logger object.
#' @import lgr
#'
#' @examples
#' biodb::getLogger()
#'
#' @export
getLogger <- function() {
    return(lgr::get_logger("biodb"))
}

#' Convert a data.frame into a string.
#'
#' Prints a data frame (partially if too big) into a string.
#'
#' @param x The data frame object.
#' @param rowCut The maximum of rows to print.
#' @param colCut The maximum of columns to print.
#' @return A string containing the data frame representation (or part of it).
#'
#' @examples
#' # Converts the first 5 rows and first 6 columns of a data frame into a
#' # string:
#' x <- data.frame(matrix(1:160, nrow=10, byrow=TRUE))
#' s <- df2str(x, rowCut=5, colCut=6)
#'
#' @export
df2str <- function(x, rowCut=5, colCut=5) {

    size <- ''

    if (is.null(x))
        s <- 'NULL'
    else if ( ! is.data.frame(x))
        s <- 'not a dataframe'
    else {
        size <- paste0('[', nrow(x), ', ', ncol(x), ']')
        colNames <- if (ncol(x) > colCut)
            c(colnames(x)[seq_len(colCut)], '...') else colnames(x)
        s <- paste0('[', paste(colNames, collapse=', '), ']')
        for (nRow in seq_len(min(rowCut, nrow(x)))) {
            rowValues <- if (ncol(x) > colCut)
                c(x[nRow, seq_len(colCut)], '...') else x[nRow, ]
            s <- paste0(s, ' [', paste(rowValues, collapse=', '), ']')
        }
        if (nrow(x) > rowCut)
            s <- paste(s, '...')
    }
    
    if (size != '')
        s <- paste0(size, ': ', s)
    
    return(s)
}

#' Convert a list into a string.
#'
#' Prints a string (partially if too big) into a string.
#'
#' @param x The list to convert into a string.
#' @param nCut The maximum of elements to print.
#' @return A string containing the list representation (or part of it).
#'
#' @examples
#' # Converts the first 5 elements of a list into a string:
#' s <- lst2str(1:10, nCut=5)
#'
#' @export
lst2str <- function(x, nCut=10) {

    if (length(x) == 0)
        s <- 'none'
    else {
        s <- paste(if (length(x) > nCut) c(x[seq_len(nCut)], '...') else x,
            collapse=", ")
        s <- paste0('"', s, '"')
        s <- paste0('[', length(x), ']: ', s)
    }
    
    return(s)
}

#' Log information message.
#'
#' Logs an information level message with biodb logger.
#'
#' @param ... Values to be passed to sprintf().
#' @return Nothing.
#'
#' @examples
#' # Logs an info message:
#' biodb::logInfo('Index is %d.', 10)
#'
#' @export
logInfo <- function(...) {
    getLogger()$info(..., caller=lgr::get_caller(-9L))
    return(invisible(NULL))
}

#' Log information message.
#'
#' Logs an information level message with biodb logger, using paste0().
#'
#' @param ... Values to be passed to paste0().
#' @return Nothing.
#'
#' @examples
#' # Logs an info message:
#' biodb::logInfo0('Index is ', 10, '.')
#'
#' @export
logInfo0 <- function(...) {
    getLogger()$info(paste0(...), caller=lgr::get_caller(-9L))
    return(invisible(NULL))
}

#' Log trace message.
#'
#' Logs a trace level message with biodb logger.
#'
#' @param ... Values to be passed to sprintf().
#' @return Nothing.
#'
#' @examples
#' # Logs a trace message:
#' biodb::logTrace('Index is %d.', 10)
#'
#' @export
logTrace <- function(...) {
    getLogger()$trace(..., caller=lgr::get_caller(-9L))
    return(invisible(NULL))
}

#' Log trace message.
#'
#' Logs a trace level message with biodb logger, using paste0().
#'
#' @param ... Values to be passed to paste0() 
#' @return Nothing.
#'
#' @examples
#' # Logs a trace message:
#' biodb::logTrace0('Index is ', 10, '.')
#'
#' @export
logTrace0 <- function(...) {
    getLogger()$trace(paste0(...), caller=lgr::get_caller(-9L))
    return(invisible(NULL))
}

#' Log debug message.
#'
#' Logs a debug level message with biodb logger.
#'
#' @param ... Values to be passed to sprintf().
#' @return Nothing.
#'
#' @examples
#' # Logs a debug message:
#' biodb::logDebug('Index is %d.', 10)
#'
#' @export
logDebug <- function(...) {
    getLogger()$debug(..., caller=lgr::get_caller(-9L))
    return(invisible(NULL))
}

#' Log debug message.
#'
#' Logs a debug level message with biodb logger, using paste0().
#'
#' @param ... Values to be passed to paste0() 
#' @return Nothing.
#'
#' @examples
#' # Logs a debug message:
#' biodb::logDebug0('Index is ', 10, '.')
#'
#' @export
logDebug0 <- function(...) {
    getLogger()$debug(paste0(...), caller=lgr::get_caller(-9L))
    return(invisible(NULL))
}

#' Throw a warning and log it too.
#'
#' Throws a warning and logs it too with biodb logger.
#'
#' @param ... Values to be passed to sprintf().
#' @return Nothing.
#'
#' @examples
#' # Throws a warning:
#' tryCatch(biodb::warn('Index is %d.', 10), warning=function(w){w$message})
#'
#' @export
warn <- function(...) {
    getLogger()$warn(..., caller=lgr::get_caller(-9L))
    warning(sprintf(...))
}

#' Throw a warning and log it too.
#'
#' Throws a warning and logs it too with biodb logger, using paste0().
#'
#' @param ... Values to be passed to paste0().
#' @return Nothing.
#'
#' @examples
#' # Throws a warning:
#' tryCatch(biodb::warn0('Index is ', 10, '.'), warning=function(w){w$message})
#'
#' @export
warn0 <- function(...) {
    msg <- paste0(...)
    getLogger()$warn(msg, caller=lgr::get_caller(-9L))
    warning(msg)
}

#' Throw an error and log it too.
#'
#' Throws am error and logs it too with biodb logger.
#'
#' @param ... Values to be passed to sprintf().
#' @return Nothing.
#'
#' @examples
#' # Throws an error:
#' tryCatch(biodb::error('Index is %d.', 10), error=function(e){e$message})
#'
#' @export
error <- function(...) {
    getLogger()$error(..., caller=lgr::get_caller(-9L))
    stop(sprintf(...))
}

#' Throw an error and log it too.
#'
#' Throws an error and logs it too with biodb logger, using paste0().
#'
#' @param ... Values to be passed to paste0().
#' @return Nothing.
#'
#' @examples
#' # Throws an error:
#' tryCatch(biodb::error0('Index is ', 10, '.'), error=function(e){e$message})
#'
#' @export
error0 <- function(...) {
    msg <- paste0(...)
    getLogger()$error(msg, caller=lgr::get_caller(-9L))
    stop(msg)
}

#' Get default cache folder.
#'
#' Returns the path to the default cache folder.
#'
#' @return The path to the cache folder.
#'
#' @examples
#' cacheFolderPath <- biodb::getDefaultCacheDir()
#'
#' @import tools
#' @export
getDefaultCacheDir <- function() {
    return(tools::R_user_dir('biodb', which="cache"))
}

#' Check deprecated default cache folders.
#'
#' Searches for a deprecated location of the default cache folder, and moves
#' files to the new location if possible. Otherwise raises a warning.
#'
#' @examples
#' biodb::checkDeprecatedCacheFolders()
#'
#' @return Nothing.
#'
#' @import rappdirs
#' @export
checkDeprecatedCacheFolders  <- function() {

    # Get folders
    newFolder <- getDefaultCacheDir()
    oldFolders <- rappdirs::user_cache_dir("biodb")
    env <- Sys.getenv()
    if ('HOME' %in% names(env))
        oldFolders <- c(oldFolders, file.path(env[['HOME']], '.biodb.cache'))

    # Check if some deprecated default folder still exists
    for (oldFolder in oldFolders)
        if (file.exists(oldFolder)) {

            # New folder already in use
            if (file.exists(newFolder))
                warn0('A deprecated default cache folder ("', oldFolder,
                    '") is still present on this machine, ',
                    'while new default cache folder "', newFolder, 
                    '" is already in used. Please, consider removing the old ',
                    'location since it has no utility anymore.')

            # Move deprecated folder to new location
            else {
                if ( ! dir.exists(dirname(newFolder)))
                    dir.create(dirname(newFolder), recursive=TRUE)
                file.rename(oldFolder, newFolder)
                logInfo0('Cache folder location has been moved from "',
                    oldFolder, '" to "', newFolder, '".')
            }
        }

    return(invisible(NULL))
}
