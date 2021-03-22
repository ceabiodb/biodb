#' Extension defintions file class
#'
#' @description
#' A class for generating the definitions.yml file of a new extension package.
#'
#' @details
#' This class generates the definitions.yml of a new extension package, needed
#' for definining the new connector.
#'
#' @import R6
#' @include ExtFileGenerator.R
#' @export
ExtDefinitions <- R6::R6Class('ExtDefinitions',
                              
inherit=ExtFileGenerator,

public=list(
         
#' @description
#' Constructor
#' @param ... See the constructor of ExtFileGenerator for the parameters.
#' offers this possiblity.
initialize=function(...) {
    super$initialize(template='definitions.yml', folder='inst',
                     filename='definitions.yml', ...)
}
))
