#' Extension license
#'
#' @description
#' A class for generating or upgrading the license of a biodb extension package.
#'
#' @details
#' This class generates the license for a new extension package, or update the
#' license of an existing one.
#'
#' @import R6
#' @include ExtFileGenerator.R
#' @export
ExtLicense <- R6::R6Class('ExtLicense',

inherit=ExtFileGenerator,

public=list(
         
#' @description
#' Constructor
#' @param ... See the constructor of ExtFileGenerator for the parameters.
#' @return A new instance.
initialize=function(...) {
    super$initialize(filename='LICENSE', ...)
    private$template <- paste0(private$pkgLicense, '.txt')
}
))
