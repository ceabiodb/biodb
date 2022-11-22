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

inherit=ExtGenerator,

public=list(

#' @description
#' Initializer.
#' @param ... See the constructor of ExtGenerator for the parameters.
#' @return Nothing.
initialize=function(...) {
    super$initialize(...)
    chk::chk_dir(private$path)
    private$makefileGen <- private$createGenerator(ExtFileGenerator,
        template='make_file', filename='Makefile')

    return(invisible(NULL))
}
),

private=list(
    makefileGen=NULL

,doGenerate=function(overwrite=FALSE, fail=TRUE) {
    private$makefileGen$generate(overwrite=overwrite, fail=fail)
}

,doUpgrade=function(generate=TRUE) {
    private$makefileGen$upgrade(generate=generate)
}
))
