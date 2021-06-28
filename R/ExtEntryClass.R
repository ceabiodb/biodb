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
#' @examples
#' # Generate a new entry class inside R folder:
#' pkgFolder <- file.path(tempfile(), 'biodbFoo')
#' dir.create(pkgFolder, recursive=TRUE)
#' biodb::ExtEntryClass$new(path=pkgFolder, dbName='foo.db',
#'                          dbTitle='Foo database',
#'                          connType='mass', entryType='xml')$generate()
#'
#' @import R6
#' @include ExtFileGenerator.R
#' @export
ExtEntryClass <- R6::R6Class('ExtEntryClass',

inherit=ExtFileGenerator,

public=list(

#' @description
#' Initializer.
#' @param ... See the constructor of ExtFileGenerator for the parameters.
#' @return Nothing.
initialize=function(...) {
    super$initialize(template='Entry.R', folder='R', ...)
    private$filename <- paste0(getEntryClassName(private$tags$dbName), '.R')

    return(invisible(NULL))
}
))
