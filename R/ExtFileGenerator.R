#' Extension file generator abstract class
#'
#' @description
#' The mother class of all file generators for biodb extension packages.
#'
#' @details
#' All file generators for biodb extensions must inherit from this class.
#'
#' @import R6
#' @import chk
#' @export
ExtFileGenerator <- R6::R6Class('ExtFileGenerator',

public=list(
         
#' @description
#' Constructor
#' @param path      The path to the package folder.
#' @param template  The filename of the template to use.
#' @param folder    The destination subfolder inside the package directory, as
#' a character vector of subfolders hierarchy.
#' @param filename  The name of the generated file.
initialize=function(path, filename, folder=character(), template=NULL) {
    chk::chk_dir(path)
    chk::chk_null_or(template, chk::chk_string)
    chk::chk_character(folder)
    chk::chk_not_any_na(folder)

    private$path <- normalizePath(path)
    private$template <- template
    private$folder <- folder
    private$filename <- filename
}

),

private=list(
    path=NULL
    ,template=NULL
    ,folder=NULL
    ,filename=NULL

,getTemplateFile=function() {
    
    templFile <- NULL

    if ( ! is.null(private$template))
        templFile <- system.file('templates', private$template,
                                 package='biodb', mustWork=TRUE)
    
    return(templFile)
}

,getDstFile=function() {
    dst <- file.path(getFolderFromVect(c(private$path, private$folder)),
                     private$filename)
    chk::chk_false(chk::vld_file(dst))
    return(dst)
}
))
