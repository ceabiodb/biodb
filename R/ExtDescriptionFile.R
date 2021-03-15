#' A class for generating a DESCRIPTION file for an extension package.
#'
#' This class generates a DESCRIPTION for a biodb extension package.
#'
#' @param path      The path to the package folder.
#'
#' @examples
#'
#'
#' @import methods
#' @import chk
#' @import desc
#' @export ExtDescriptionFile
#' @exportClass ExtDescriptionFile
ExtDescriptionFile <- methods::setRefClass('ExtDescriptionFile',
    fields=list(
        path='character'
    ),

methods=list(

initialize=function(path) {
    chk::chk_dir(path)

    .self$path <- normalizePath(path)
},

generate=function() {
    ":\n\nGenerates the DESCRIPTION file inside the package folder.
    \nReturned value: None.
    "
    
    pkgName <- basename(.self$path)
    descFile <- desc::description$new("!new")
    descFile$set("Package", pkgName)
    descFile$write(file=file.path(.self$path, 'DESCRIPTION'))
}

))
