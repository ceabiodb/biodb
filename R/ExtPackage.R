#' Extension package class
#'
#' @description
#' A class for generating the skeleton of a new extension package.
#'
#' @details
#' This class manages the files of an extension package.
#'
#' It can generate all the files of a new extension package: DESCRIPTION,
#' NEWS, README.md, tests, definitons.yml, etc. Optionnaly it also generates
#' other files like: a `.travis.yml` file for Travis-CI, a Makefile for easing
#' development on UNIX-like platforms outside of Rstudio.
#'
#' It can also upgrade files of an existing package like: definitions.yml,
#' Makefile, .travis.yml, LICENSE, etc.
#'
#' @examples
#' # Generate a new package:
#' pkgFolder <- file.path(tempfile(), 'biodbFoo')
#' dir.create(pkgFolder, recursive=TRUE)
#' biodb::ExtPackage$new(path=pkgFolder, dbName='foo.db',
#'                         dbTitle='Foo database', rcpp=TRUE,
#'                         connType='mass', entryType='txt', downloadable=TRUE,
#'                         remote=TRUE)$generate()
#'
#' @import R6
#' @import chk 
#' @include ExtGenerator.R
#' @export
ExtPackage <- R6::R6Class('ExtPackage',

inherit=ExtGenerator,

public=list(
         
#' @description
#' Constructor
#' @param ... See the constructor of ExtGenerator for the parameters.
#' @return A new instance.
initialize=function(...) {
    super$initialize(...)
}
),

private=list(
doGenerate=function(overwrite=FALSE, fail=TRUE) {
    
    if ( ! dir.exists(private$path))
        dir.create(private$path, recursive=TRUE)

    private$createGenerator(ExtDescriptionFile)$generate(overwrite=overwrite,
                                                         fail=fail)
    if (private$tags$makefile)
        private$createGenerator(ExtMakefile)$generate(overwrite=overwrite,
                                                      fail=fail)
    private$createGenerator(ExtLicense)$generate(overwrite=overwrite, fail=fail)
    private$createGenerator(ExtReadme)$generate(overwrite=overwrite, fail=fail)
    if ( ! is.null(private$tags$dbName)) {
        private$createGenerator(ExtConnClass)$generate(overwrite=overwrite,
                                                       fail=fail)
        private$createGenerator(ExtEntryClass)$generate(overwrite=overwrite,
                                                        fail=fail)
        private$createGenerator(ExtDefinitions)$generate(overwrite=overwrite,
                                                         fail=fail)
    }
    private$createGenerator(ExtPackageFile)$generate(overwrite=overwrite,
                                                     fail=fail)
    if (private$tags$rcpp)
        private$createGenerator(ExtCpp)$generate(overwrite=overwrite, fail=fail)
    private$createGenerator(ExtRbuildignore)$generate(overwrite=overwrite,
                                                      fail=fail)
    private$createGenerator(ExtGitignore)$generate(overwrite=overwrite,
                                                      fail=fail)
    private$createGenerator(ExtTravisFile)$generate(overwrite=overwrite,
                                                    fail=fail)
    private$createGenerator(ExtTests)$generate(overwrite=overwrite, fail=fail)
    private$createGenerator(ExtVignette)$generate(overwrite=overwrite,
                                                  fail=fail)
}

,doUpgrade=function(generate=TRUE) {

    chk::chk_dir(private$path)

    # Upgradable files
    if (private$tags$makefile)
        private$createGenerator(ExtMakefile)$upgrade(generate=generate)
    private$createGenerator(ExtRbuildignore)$upgrade(generate=generate)
    private$createGenerator(ExtGitignore)$upgrade(generate=generate)

    # Non-upgradable files
    if (generate) {
        private$createGenerator(ExtDescriptionFile)$generate(fail=FALSE)
        private$createGenerator(ExtLicense)$generate(fail=FALSE)
        private$createGenerator(ExtReadme)$generate(fail=FALSE)
        if ( ! is.null(private$tags$dbName)) {
            private$createGenerator(ExtConnClass)$generate(fail=FALSE)
            private$createGenerator(ExtEntryClass)$generate(fail=FALSE)
            private$createGenerator(ExtDefinitions)$generate(fail=FALSE)
        }
        private$createGenerator(ExtPackageFile)$generate(fail=FALSE)
        if (private$tags$rcpp)
            private$createGenerator(ExtCpp)$generate(fail=FALSE)
        private$createGenerator(ExtTravisFile)$generate(fail=FALSE)
        private$createGenerator(ExtTests)$generate(fail=FALSE)
        private$createGenerator(ExtVignette)$generate(fail=FALSE)
    }
}
))
