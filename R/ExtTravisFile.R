#' Extension Travis YAML file generator class
#'
#' @description
#' A class for generating a .travis.yml file for a new extension package.
#'
#' @examples
#' # Generate a new package:
#' pkgFolder <- file.path(tempfile(), 'biodbFoo')
#' dir.create(pkgFolder, recursive=TRUE)
#' biodb::ExtTravisFile$new(path=pkgFolder, email='myname@e.mail')$generate()
#'
#' @details
#' Write a .travis.yml file inside the package directory, using a template file.
#'
#' @import R6
#' @include ExtFileGenerator.R
#' @export
ExtTravisFile <- R6::R6Class('ExtTravisFile',

inherit=ExtFileGenerator,

public=list(

#' @description
#' Constructor
#' @param ... See the constructor of ExtFileGenerator for the parameters.
#' @return A new instance.
initialize=function(...) {
    super$initialize(template='travis.yml', filename='.travis.yml', ...)
}
))
