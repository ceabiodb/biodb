#' Extension Gitignore file class
#'
#' @description
#' A class for generating the .gitignore file of an extension package.
#'
#' @details
#' This class can be used to generate a new .gitignore file or to keep one
#' up to date.
#'
#' @examples
#' # Generate a new package:
#' pkgFolder <- file.path(tempfile(), 'biodbFoo')
#' dir.create(pkgFolder, recursive=TRUE)
#' biodb::ExtGitignore$new(path=pkgFolder)$generate()
#'
#' @import R6
#' @import chk
#' @include ExtFileGenerator.R
#' @export
ExtGitignore <- R6::R6Class('ExtGitignore',

inherit=ExtFileGenerator,

public=list(

#' @description
#' Constructor
#' @param ... See the constructor of ExtFileGenerator for the parameters.
#' @return A new instance.
initialize=function(...) {
    super$initialize(filename=".gitignore", template='gitignore',
        upgrader='lineAdder', ...)
}
))

