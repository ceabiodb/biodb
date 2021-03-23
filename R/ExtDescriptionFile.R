#' Extension DESCRIPTION file
#'
#' @description
#' A class for generating a DESCRIPTION file for an extension package.
#'
#' @details
#' This class generates a DESCRIPTION for a biodb extension package.
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
    if (is.null(private$firstname))
        private$firstname <- 'Firstname of author'
    if (is.null(private$lastname))
        private$lastname <- 'Lastname of author'
    if (is.null(private$email))
        private$email <- 'author@e.mail'
}
))
