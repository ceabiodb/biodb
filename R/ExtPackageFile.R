#' Extension package file class.
#'
#' @description
#' A class for generating the package.R file for a biodb extension.
#'
#' @details
#' This class generates the package.R file, writing a reference to the
#' generated skeleton vignette, and possibly including directives for C++ code.
#'
#' @import R6
#' @import chk
#' @export
ExtPackageFile <- R6::R6Class('ExtPackageFile',

public=list(
         
#' @description
#' Constructor
#' @param path      The path to the package folder.
#' @param dbName    The name of the database (in biodb format "my.db.name"),
#' that will be used in "definitions.yml" file and for connector and entry
#' classes.
#' @param vignetteName Set to the name of the default/main vignette.
#' @param rcpp      Set to TRUE to enable Rcpp C/C++ code inside the package.
#' @return A new instance.
initialize=function(path, dbName=NULL, vignetteName=NULL, rcpp=FALSE) {
    chk::chk_dir(path)
    chk::chk_null_or(dbName, chk::chk_string)
    chk::chk_null_or(vignetteName, chk::chk_string)
    chk::chk_flag(rcpp)
    
    private$path <- normalizePath(path)
    private$dbName <- dbName
    private$vignetteName <- vignetteName
    private$rcpp <- rcpp
}

#' @description
#' Generates R/package.R file.
,generate=function() {
    temp <- FileTemplate$new(system.file('templates', 'package.R',
                                         package='biodb'))
    temp$replace('pkgName', getPkgName(private$path))
    if ( ! is.null(private$dbName))
        temp$replace('connClassName', getConnClassName(private$dbName))
    if ( ! is.null(private$vignetteName))
        temp$replace('vignette', private$vignetteName)
    temp$select('compile', private$rcpp)
    temp$write(file.path(getRFolder(private$path), 'package.R'))
}
),

private=list(
    path=NULL
    ,dbName=NULL
    ,vignetteName=NULL
    ,rcpp=NULL
))
