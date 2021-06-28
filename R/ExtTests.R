#' Extension tests class
#'
#' @description
#' A class for generating test files.
#'
#' @details
#' This class generates a test file for running biodb generic tests, and a test
#' file containing an example of a custom test for this extension.
#'
#' @examples
#' # Generate a new package:
#' pkgFolder <- file.path(tempfile(), 'biodbFoo')
#' dir.create(pkgFolder, recursive=TRUE)
#' biodb::ExtTests$new(path=pkgFolder, dbName='foo.db', rcpp=TRUE,
#'                     remote=TRUE)$generate()
#'
#' @import R6
#' @include ExtGenerator.R
#' @export
ExtTests <- R6::R6Class('ExtTests',

inherit=ExtGenerator,

public=list(

#' @description
#' Initializer.
#' @param ... See the constructor of ExtGenerator for the parameters.
#' @return Nothing.
initialize=function(...) {
    super$initialize(...)
    chk::chk_dir(private$path)

    return(invisible(NULL))
}
),

private=list(
doGenerate=function(overwrite=FALSE, fail=TRUE) {

    # Generate tests
    private$generateDefaultTests(overwrite=overwrite, fail=fail)
    private$generateLongTests(overwrite=overwrite, fail=fail)
    private$generateResFile(overwrite=overwrite, fail=fail)
}

,generateResFile=function(overwrite=FALSE, fail=TRUE) {

    # Generate reference file resource if none exists
    resFolder <- c('inst', 'testref')
    resPath <- getFolderFromVect(c(private$path, resFolder))
    if (dir.exists(resPath) && length(Sys.glob(file.path(resPath, '/*'))) == 0)
        private$createGenerator(ExtFileGenerator, template='entry-0001.json',
            folder=resFolder, filename=paste('entry', private$tags$dbName,
            '0001.json', sep='-'))$generate(overwrite=overwrite, fail=fail)
}

,generateLongTests=function(overwrite=FALSE, fail=TRUE) {

    # Generate testthat.R script
    private$createGenerator(ExtFileGenerator, template='testthat.R',
        folder='longtests', filename='testthat.R')$generate(overwrite=overwrite,
        fail=fail)

    # Generate long tests
    longTestFolder <- c('longtests', 'testthat')
    longTestPath <- getFolderFromVect(c(private$path, longTestFolder))
    if (dir.exists(longTestPath)
        && length(Sys.glob(file.path(longTestPath, '/*init_logging*.R'))) == 0)
    {
        private$createGenerator(ExtFileGenerator,
            template='test_long_001_init_logging.R', folder=longTestFolder,
            filename='test_long_001_init_logging.R')$generate(
            overwrite=overwrite, fail=fail)
        private$createGenerator(ExtFileGenerator,
            template='test_long_100_generic.R', folder=longTestFolder,
            filename='test_long_100_generic.R')$generate(overwrite=overwrite,
            fail=fail)
        private$createGenerator(ExtFileGenerator,
            template='test_long_200_example.R', folder=longTestFolder,
            filename='test_long_200_example.R')$generate(overwrite=overwrite,
            fail=fail)
    }
}

,generateDefaultTests=function(overwrite=FALSE, fail=TRUE) {

    # Generate building options
    private$createGenerator(ExtFileGenerator, template='BBSoptions',
        filename='.BBSoptions')$generate(overwrite=overwrite, fail=fail)

    # Generate testthat.R script
    private$createGenerator(ExtFileGenerator, template='testthat.R',
        folder='tests', filename='testthat.R')$generate(overwrite=overwrite,
        fail=fail)

    # Generate R test files if no init logging file exists
    testthatFolder <- c('tests', 'testthat')
    testthatPath <- getFolderFromVect(c(private$path, testthatFolder))
    if (dir.exists(testthatPath)
        && length(Sys.glob(file.path(testthatPath, '/*init_logging*.R'))) == 0)
    {
        private$createGenerator(ExtFileGenerator,
            template='test_001_init_logging.R', folder=testthatFolder,
            filename='test_001_init_logging.R')$generate(overwrite=overwrite,
            fail=fail)
        private$createGenerator(ExtFileGenerator, template='test_050_fcts.R',
            folder=testthatFolder,
            filename='test_050_fcts.R')$generate(overwrite=overwrite,
            fail=fail)
        private$createGenerator(ExtFileGenerator, template='test_100_generic.R',
            folder=testthatFolder,
            filename='test_100_generic.R')$generate(overwrite=overwrite,
            fail=fail)
        private$createGenerator(ExtFileGenerator, template='test_200_example.R',
            folder=testthatFolder,
            filename='test_200_example.R')$generate(overwrite=overwrite,
            fail=fail)
    }
}
))
