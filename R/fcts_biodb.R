#' Get connector class name.
#'
#' Gets the name of the connector class corresponding to a connector.
#'
#' @param connName A connector name (e.g.: "mass.csv.file").
#' @return The name of the corresponding connector class (e.g.:
#' "MassCsvFileConn").
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
#' @export
getEntryClassName <- function(connName) {
    return(paste0(connNameToClassPrefix(connName), 'Entry'))
}

#' Get connector types.
#'
#' Get the list of available connector types.
#'
#' @return A character vector containing the connector types.
#' @export
getConnTypes <- function() {
    return(c('plain', 'compound', 'mass'))
}

#' Get entry types.
#'
#' Get the list of available entry types.
#'
#' @return A character vector containing the entry types.
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
#' @export
df2str <- function(x, rowCut=5, colCut=5) {

    size <- ''

    if (is.null(x))
        s <- 'NULL'
    else if ( ! is.data.frame(x))
        s <- 'not a dataframe'
    else {
        size <- paste0('[', nrow(x), ', ', ncol(x), ']')
        colNames <- if (ncol(x) > colCut) c(colnames(x)[seq_len(colCut)], '...') else colnames(x)
        s <- paste0('[', paste(colNames, collapse=', '), ']')
        for (nRow in seq_len(min(rowCut, nrow(x)))) {
            rowValues <- if (ncol(x) > colCut) c(x[nRow, seq_len(colCut)], '...') else x[nRow, ]
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
#' @param nCut The maximum of elements to print.
#' @return A string containing the list representation (or part of it).
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
#' @export
error0 <- function(...) {
    msg <- paste0(...)
    getLogger()$error(msg, caller=lgr::get_caller(-9L))
    stop(msg)
}
