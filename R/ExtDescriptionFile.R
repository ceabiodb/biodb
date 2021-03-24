#' Extension DESCRIPTION file
#'
#' @description
#' A class for generating a DESCRIPTION file for an extension package.
#'
#' @details
#' This class generates a DESCRIPTION for a biodb extension package.
#'
#' @examples
#' # Generate the DESCRIPTION file:
#' pkgFolder <- file.path(tempfile(), 'biodbFoo')
#' dir.create(pkgFolder, recursive=TRUE)
#' biodb::ExtDescriptionFile$new(path=pkgFolder, dbName='foo.db',
#'                               dbTitle='Foo database', email='j.smith@e.mail',
#'                               firstname='John', lastname='Smith', rcpp=TRUE,
#'                               entryType='xml')$generate()
#'
#' @import R6
#' @include ExtFileGenerator.R
#' @export
ExtDescriptionFile <- R6::R6Class('ExtDescriptionFile',

inherit=ExtFileGenerator,

public=list(

#' @description
#' Constructor.
#' @param ... See the constructor of ExtFileGenerator for the parameters.
#' @return A new instance.
initialize=function(...) {
    super$initialize(template='DESCRIPTION', filename='DESCRIPTION', ...)
}
))
