#' Extension vignette class
#'
#' @description
#' A class for generating a vignette example for an extension package.
#'
#' @details
#' This class generates a vignette file, serving as example to demonstrate the
#' use of the extension package.
#'
#' @examples
#' # Generate a new package:
#' pkgFolder <- file.path(tempfile(), 'biodbFoo')
#' dir.create(pkgFolder, recursive=TRUE)
#' biodb::ExtVignette$new(path=pkgFolder, dbName='foo.db',
#'                        dbTitle='Foo database', vignetteName='main', 
#'                        firstname='John', lastname='Smith',
#'                        remote=TRUE)$generate()
#'
#' @import R6
#' @include ExtFileGenerator.R
#' @export
ExtVignette <- R6::R6Class('ExtVignette',

inherit=ExtFileGenerator,

public=list(

#' @description
#' Constructor.
#' @param ... See the constructor of ExtFileGenerator for the parameters.
#' @return A new instance.
initialize=function(...) {
    super$initialize(template='vignette.Rmd', folder='vignettes', ...)
    private$filename <- paste0(private$tags$vignetteName, '.Rmd')
}
))
