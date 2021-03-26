#' Extension Rbuildignore file class
#'
#' @description
#' A class for generating the .Rbuildignore file of an extension package.
#'
#' @details
#' This class can be used to generate a new .Rbuildignore file or to keep one
#' up to date.
#'
#' @examples
#' # Generate a new package:
#' pkgFolder <- file.path(tempfile(), 'biodbFoo')
#' dir.create(pkgFolder, recursive=TRUE)
#' biodb::ExtRbuildignore$new(path=pkgFolder)$generate()
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
#' @param ... See the constructor of ExtFileGenerator for the parameters.
#' @return A new instance.
initialize=function(...) {
    super$initialize(filename=".Rbuildignore", template='Rbuildignore', ...)
}

#' @description
#' Generates the Rbuildignore file for the specified package.
#' @return None.
,generate=function(overwrite=FALSE, fail=TRUE) {
    
    if ( ! overwrite && file.exists(private$getDstFile())) {
        if (fail)
            stop('Unable to generate "', private$getDstFile(), '", file already exists.')

    } else
        file.copy(private$getTemplateFile(), private$getDstFile(),
                  overwrite=overwrite) 
    
    return(invisible(NULL))
}

#' @description
#' Upgrades an existing Rbuildignore file for the specified package.
#' @return None.
,upgrade=function() {

    dst <- private$getDstFile(exist=TRUE)
    
    # Read lines from templates and destinationl file
    templLines <- readLines(private$getTemplateFile())
    dstLines <- readLines(dst)
    
    # Add missing lines in destination file
    for (tLine in templLines)
        if ( ! tLine %in% dstLines)
            dstLines <- c(dstLines, tLine)
    
    # Sort
    dstLines <- sort(dstLines)
    
    # Write destination file
    writeLines(dstLines, dst)

    return(invisible(NULL))
}
))
