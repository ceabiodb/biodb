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
#' @import R6
#' @import chk
#' @export
ExtCpp <- R6::R6Class('ExtCpp',

public=list(
         
#' @description
#' Constructor
#' @param path      The path to the package folder.
#' @return A new instance.
initialize=function(path) {
    chk::chk_string(path)

    private$path <- normalizePath(path, mustWork=FALSE)
}

#' @description
#' Generates examples of C++ code.
#'
,generate=function() {
    src <- getSrcFolder(private$path)
    templates <- system.file('templates', package='biodb')
    for (f in Sys.glob(paste0(templates, '/*.cpp')))
        file.copy(f, src)
}
),

private=list(
    path=NULL
))
