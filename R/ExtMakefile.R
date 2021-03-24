#' Extension Makefile
#'
#' @description
#' A class for generating a Makefile for an extension package.
#'
#' @details
#' This class generates a Makefile, usable on UNIX-like platforms, for managing
#' a biodb extension package. Targets are automatically generated for running
#' CRAN check, Bioconductor check, testthat tests, compiling, generating
#' documentation, cleaning, etc.
#'
#' @examples
#' # Generate a new connector class inside R folder:
#' pkgFolder <- file.path(tempfile(), 'biodbFoo')
#' dir.create(pkgFolder, recursive=TRUE)
#' biodb::ExtMakefile$new(path=pkgFolder)$generate()
#'
#' @import R6
#' @include ExtFileGenerator.R
#' @export
ExtMakefile <- R6::R6Class('ExtMakefile',

inherit=ExtFileGenerator,

public=list(

#' @description
#' Constructor.
#' @param ... See the constructor of ExtFileGenerator for the parameters.
#' @return A new instance.
initialize=function(...) {
    super$initialize(template='Makefile', filename='Makefile', ...)
}
))
