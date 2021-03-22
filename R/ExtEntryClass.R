#' Extension entry class
#'
#' @description
#' A class for generating a new entry class.
#'
#' @details
#' This class generates a new entry class from given parameters.
#' The new class can inherit directly from \code{BiodbEntry} or
#' from one of its sub-classes: \code{BiodbCsvEntry}, \code{BiodbHtmlEntry}, ...
#'
#' @import R6
#' @include ExtFileGenerator.R
#' @export
ExtEntryClass <- R6::R6Class('ExtEntryClass',

inherit=ExtFileGenerator,

public=list(
         
#' @description
#' Constructor
#' @param ... See the constructor of ExtFileGenerator for the parameters.
#' @return A new instance.
initialize=function(...) {
    super$initialize(template='Entry.R', folder='R', ...)
    chk::chk_string(private$dbName)
    private$filename <- paste0(getEntryClassName(private$dbName), '.R')
}
))
