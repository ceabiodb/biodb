#' Extension README file class
#'
#' @description
#' A class for generating a README file for a new extension package.
#'
#' @details
#' Write a README file inside package directory, using a template file.
#'
#' @examples
#' # Generate a new package:
#' pkgFolder <- file.path(tempfile(), 'biodbFoo')
#' dir.create(pkgFolder, recursive=TRUE)
#' biodb::ExtReadme$new(path=pkgFolder, dbName='foo.db',
#'                      dbTitle='Foo database')$generate()
#'
#' @import R6
#' @include ExtFileGenerator.R
#' @export
ExtReadme <- R6::R6Class('ExtReadme',

inherit=ExtFileGenerator,

public=list(

#' @description
#' Constructor
#' @param ... See the constructor of ExtFileGenerator for the parameters.
#' @return A new instance.
initialize=function(...) {
    super$initialize(template='README.md', filename='README.md', ...)
}
))
