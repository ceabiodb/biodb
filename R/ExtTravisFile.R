#' Extension Travis YAML file generator class
#'
#' @description
#' A class for generating a .travis.yml file for a new extension package.
#'
#' @details
#' Write a .travis.yml file inside the package directory, using a template file.
#'
#' @import R6
#' @include ExtFileGenerator.R
ExtTravisFile <- R6::R6Class('ExtTravisFile',

inherit=ExtFileGenerator,

public=list(

#' @description
#' Constructor
#' @param path     The path to the package folder.
#' @param pkgName   The package name. If set to NULL, the folder name pointer by
#' the "path" paramater will be used as the package name.
#' @param email     The email of the author.
#' @return A new instance.
initialize=function(...) {
    super$initialize(template='travis.yml', filename='.travis.yml', ...)
}
))
