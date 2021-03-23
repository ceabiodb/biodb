#' Extension tests class
#'
#' @description
#' A class for generating test files.
#'
#' @details
#' This class generates a test file for running biodb generic tests, and a test
#' file containing an example of a custom test for this extension.
#'
#' @import R6
#' @include ExtGenerator.R
#' @export
ExtTests <- R6::R6Class('ExtTests',

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
,generate=function() {
    private$createGenerator(ExtFileGenerator, template='testthat.R',
                            folder='tests', filename='testthat.R')$generate()
    private$createGenerator(ExtFileGenerator, template='test_100_generic.R',
                            folder=c('tests', 'testthat'),
                            filename='test_100_generic.R')$generate()
    private$createGenerator(ExtFileGenerator, template='entry-0001.json',
                            folder=c('tests', 'testthat', 'res'),
                            filename=paste('entry', private$dbName,
                                           '0001.json', sep='-'))$generate()
}
))
