#' Extension Rbuildignore file class
#'
#' @description
#' A class for generating the .Rbuildignore file of an extension package.
#'
#' @details
#' This class can be used to generate a new .Rbuildignore file or to keep one
#' up to date.
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
#' @param path      The path to the package folder.
#' @return A new instance.
initialize=function(path) {
    super$initialize(path, filename=".Rbuildignore")
}

#' @description
#' Generates the Rbuildignore file for the specified package.
,generate=function() {
}
),

private=list(
))
