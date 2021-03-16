#' Extension README file class
#'
#' @description
#' A class for generating a README file for a new extension package.
#'
#' @details
#' Write a README file inside package directory, using a template file.
#'
#' @import R6
#' @import chk
ExtReadme <- R6::R6Class('ExtReadme',

public=list(

#' @description
#' Constructor
#' @param path     The path to the package folder.
#' @return A new instance.
initialize=function(path) {
    chk::chk_dir(path)

    private$path <- normalizePath(path)
},

#' @description
#' Generates README file.
generate=function() {
    temp <- FileTemplate$new(system.file('templates', 'README.md',
                                         package='biodb'))
    temp$replace('pkgName', private$getPkgName())
    temp$write(file.path(private$path, 'README.md'))
}
),

private=list(
    path=NULL,

getPkgName=function() {
    return(basename(private$path))
}
))
