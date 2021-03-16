#' Extension license
#'
#' @description
#' A class for generating or upgrading the license of a biodb extension package.
#'
#' @details
#' This class generates the license for a new extension package, or update the
#' license of an existing one.
#'
#' @import R6
#' @import chk
#' @export
ExtLicense <- R6::R6Class('ExtLicense',

public=list(
         
#' @description
#' Constructor
#' @param path     The path to the package folder.
#' @return A new instance.
initialize=function(path) {
    chk::chk_string(path)
    
    private$path <- normalizePath(path)
},

#' @description
#' Generates the license file and set the license name inside the DESCRIPTION
#' file.
generate=function() {
    
    # Set license name into DESCRIPTION file
    descFile <- desc::description$new(file.path(private$path, 'DESCRIPTION'))
    descFile$set(License='AGPL-3')
    descFile$write()
    
    # Copy file
    licenseFile <- system.file('templates', 'AGPL-3.txt', package='biodb')
    file.copy(licenseFile, file.path(private$path, 'LICENSE'))
}
),

private=list(
    path=NULL
))
