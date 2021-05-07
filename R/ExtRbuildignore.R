#' Extension Rbuildignore file class
#'
#' @description
#' A class for generating the .Rbuildignore file of an extension package.
#'
#' @details
#' This class can be used to generate a new .Rbuildignore file or to keep one
#' up to date.
#'
#' @examples
#' # Generate a new package:
#' pkgFolder <- file.path(tempfile(), 'biodbFoo')
#' dir.create(pkgFolder, recursive=TRUE)
#' biodb::ExtRbuildignore$new(path=pkgFolder)$generate()
#'
#' @import R6
#' @import chk
#' @include ExtFileGenerator.R
#' @export
ExtRbuildignore <- R6::R6Class('ExtRbuildignore',

inherit=ExtFileGenerator,

public=list(

#' @description
#' Constructor
#' @param ... See the constructor of ExtFileGenerator for the parameters.
#' @return A new instance.
initialize=function(...) {
    super$initialize(filename=".Rbuildignore", template='Rbuildignore',
        upgrader='lineAdder', ...)
}
))
