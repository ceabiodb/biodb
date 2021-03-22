#' Extension package file class.
#'
#' @description
#' A class for generating the package.R file for a biodb extension.
#'
#' @details
#' This class generates the package.R file, writing a reference to the
#' generated skeleton vignette, and possibly including directives for C++ code.
#'
#' @import R6
#' @include ExtFileGenerator.R
#' @export
ExtPackageFile <- R6::R6Class('ExtPackageFile',

inherit=ExtFileGenerator,

public=list(
         
#' @description
#' Constructor
#' @param ... See the constructor of ExtFileGenerator for the parameters.
#' @return A new instance.
initialize=function(...) {
    super$initialize(template='package.R', folder='R', filename='package.R',
                     ...)
}
))
