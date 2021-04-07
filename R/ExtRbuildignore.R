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
),

private=list(
doGenerate=function(overwrite=FALSE, fail=TRUE) {
    
    if ( ! overwrite && file.exists(private$getDstFile())) {
        if (fail)
            stop('Unable to generate "', private$getDstFile(), '", file already exists.')

    } else
        file.copy(private$getTemplateFile(), private$getDstFile(),
                  overwrite=overwrite) 
    
    return(invisible(NULL))
}

,doUpgrade=function(generate=TRUE) {

    dst <- private$getDstFile(exist=NULL)
    
    # Upgrade
    if (file.exists(dst)) {
    
        # Read lines from templates and destination file
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
    }
    
    # Generate
    else if (generate) {
        self$generate()
    }

    return(invisible(NULL))
}
))
