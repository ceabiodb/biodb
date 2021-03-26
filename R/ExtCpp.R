#' Extension C++ code class
#'
#' @description
#' A class for generating C++ example files (code & test).
#'
#' @details
#' This class generates examples of an R function written in C++ using Rcpp,
#' of a pure C++ function used to speed up computing, and of C++ code for testing
#' the pure C++ function.
#' As for the R function written with Rcpp, it is tested inside standard
#' testthat R code. 
#'
#' @examples
#' # Generate C++ files
#' pkgFolder <- file.path(tempfile(), 'biodbFoo')
#' dir.create(pkgFolder, recursive=TRUE)
#' biodb::ExtCpp$new(path=pkgFolder)$generate()
#'
#' @import R6
#' @include ExtGenerator.R
#' @export
ExtCpp <- R6::R6Class('ExtCpp',

inherit=ExtGenerator,

public=list(
         
#' @description
#' Constructor
#' @param ... See the constructor of ExtGenerator for the parameters.
#' @return A new instance.
initialize=function(...) {
    super$initialize(...)
    chk::chk_dir(private$path)
}

#' @description
#' Generates examples of C++ code.
,generate=function(overwrite=FALSE, fail=TRUE) {
    templates <- system.file('templates', package='biodb')
    for (f in Sys.glob(paste0(templates, '/*.cpp')))
        private$createGenerator(ExtFileGenerator, template=basename(f),
                                folder='src', filename=basename(f)
                                )$generate(overwrite=overwrite, fail=fail)
}
))
