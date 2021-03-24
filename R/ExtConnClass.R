#' Extension connector clas
#'
#' @description
#' A class for generating a new connector class.
#'
#' @details
#' This class generates a new connector class from given parameters.
#' The new class can inherit directly from \code{BiodbConn} or
#' \code{BiodbCompounddbConn} or \code{BiodbMassdbConn}.
#' It can also be editable and/or writable.
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
#' Constructor
#' @param ... See the constructor of ExtFileGenerator for the parameters.
#' @return A new instance.
initialize=function(...) {
    super$initialize(template='Conn.R', folder='R', ...)
    chk::chk_string(private$dbName)
    private$filename <- paste0(getConnClassName(private$dbName), '.R')
}
))
