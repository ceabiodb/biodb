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
#' @return A new instance.
initialize=function(...) {
    super$initialize(filename=".Rbuildignore", template='Rbuildignore', ...)
}

#' @description
#' Generates the Rbuildignore file for the specified package.
,generate=function() {
    file.copy(private$getTemplateFile(), private$getDstFile()) 
}

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
}
))
