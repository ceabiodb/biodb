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
