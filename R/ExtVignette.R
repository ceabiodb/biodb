#' Extension vignette class
#'
#' @description
#' A class for generating a vignette example for an extension package.
#'
#' @details
#' This class generates a vignette file, serving as example to demonstrate the
#' use of the extension package.
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
    super$initialize(template='intro.Rmd', folder='vignettes',
                     filename='intro.Rmd', ...)
}
))
