#' A class for generating a Makefile for an extension package.
#'
#' This class generates a Makefile, usable on UNIX-like platforms, for managing
#' a biodb extension package. Targets are automatically generated for running
#' CRAN check, Bioconductor check, testthat tests, compiling, generating
#' documentation, cleaning, etc.
#'
#' @param path      The path to the package folder.
#' @param newPkg    Set to TRUE if the package is not yet on Bioconductor.
#' @param overwrite Force overwriting of existing makefile, without testing
#' versions.
#'
#' @examples
#' # Create (or replace) a Makefile for a new Bioconductor boidb extension
#' # package. The call is made while inside the package root folder:
#' biodb::ExtMakefile('.', newPkg=TRUE)$generate()
#'
#' # Create a Makefile for an existing Bioconductor package:
#' biodb::ExtMakefile('.')$generate()
#'
#' @import methods
#' @import chk 
#' @export ExtMakefile
#' @exportClass ExtMakefile
ExtMakefile <- methods::setRefClass('ExtMakefile',
    fields=list(
        path='character',
        newPkg='logical',
        overwrite='logical'
    ),

methods=list(
         
initialize=function(path, newPkg=FALSE, overwrite=FALSE) {
    chk::chk_dir(path)
    chk::chk_flag(newPkg)
    chk::chk_flag(overwrite)

    .self$path <- normalizePath(path)
    .self$newPkg <- newPkg
    .self$overwrite <- overwrite
},

getPath=function() {
    ":\n\nGet the full path to the Makefile.
    \nReturned value: The path to the Makefile.
    "
    
    return(file.path(.self$path, 'Makefile'))
},

generate=function() {
    ":\n\nGenerates the Makefile inside the package folder.
    \nReturned value: None.
    "

    makefile <- .self$getPath()
    if (file.exists(makefile))
        stop('Makefile "', makefile, '" already exists.')

    # Copy template
    template <- system.file('templates', 'Makefile', package='biodb')
    file.copy(template, makefile)
},

upgrade=function() {
    ":\n\nUpgrades the Makefile inside the package folder. Creates one if
    none exists.
    \nReturned value: None.
    "
    
    makefile <- .self$getPath()
    copy <- TRUE
    
    # Get the template makefile
    template <- system.file('templates', 'Makefile', package='biodb')
    tempver <- extractVersion(template)

    # Is there already a makefile?
    if ( ! .self$overwrite && file.exists(makefile)) {

        # Get versions of two files
        curver <- extractVersion(makefile)

        # Compare versions
        cmp <- compareVersions(curver, tempver)
        if (cmp == 0) {
            copy <- FALSE
            warning("Aborting. A local Makefile already exists with the same",
                    " version number (", curver, ").")
        }
        else if (cmp > 0) {
            copy <- FALSE
            warning("Aborting. A local Makefile already exists with a more",
                    " recent version number (", curver, " > ", tempver, ").")
        }
    }

    # Copy makefile
    if (copy) {
        message("Copy latest version (", tempver, ") of Makefile.")
        file.copy(template, makefile)
    }
}

))
