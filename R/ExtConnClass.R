#' Extension connector clas
#'
#' @description
#' A class for generating a new connector class.
#'
#' @details
#' This class generates a new connector class from given parameters.
#' The new class inherits from \code{BiodbConn}.
#' It can be defined as a compound or mass database connector, and made
#' downloadable, editable and/or writable.
#'
#' @examples
#' # Generate a new connector class inside R folder:
#' pkgFolder <- file.path(tempfile(), 'biodbFoo')
#' dir.create(pkgFolder, recursive=TRUE)
#' biodb::ExtConnClass$new(path=pkgFolder, dbName='foo.db',
#'                         dbTitle='Foo database',
#'                         connType='mass', remote=TRUE)$generate()
#'
#' @import R6
#' @include ExtFileGenerator.R
#' @export
ExtConnClass <- R6::R6Class('ExtConnClass',

inherit=ExtFileGenerator,

public=list(

#' @description
#' Initializer.
#' @param ... See the constructor of ExtFileGenerator for the parameters.
#' @return Nothing.
initialize=function(...) {
    super$initialize(template='Conn.R', folder='R', ...)
    private$filename <- paste0(getConnClassName(private$tags$dbName), '.R')

    return(invisible(NULL))
}
))
