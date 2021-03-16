#' Extension Makefile
#'
#' @description
#' A class for generating a Makefile for an extension package.
#'
#' @details
#' This class generates a Makefile, usable on UNIX-like platforms, for managing
#' a biodb extension package. Targets are automatically generated for running
#' CRAN check, Bioconductor check, testthat tests, compiling, generating
#' documentation, cleaning, etc.
#'
#' @import R6
#' @export
ExtMakefile <- R6::R6Class('ExtMakefile',
public=list(

#' @description
#' Constructor.
#' @field path      The path to the package folder.
#' @field newPkg    Set to TRUE if the package is not yet on Bioconductor.
#' @field overwrite Force overwriting of existing makefile, without testing
#' versions.
#' @return A new instance.
initialize=function(path, newPkg=FALSE, overwrite=FALSE) {
    chk::chk_dir(path)
    chk::chk_flag(newPkg)
    chk::chk_flag(overwrite)

    private$path <- normalizePath(path)
    private$newPkg <- newPkg
    private$overwrite <- overwrite
},

#' @description
#' Get the full path to the Makefile.
#' @return The path to the Makefile.
getPath=function() {
    return(file.path(private$path, 'Makefile'))
},

#' @description
#' Generates the Makefile inside the package folder.
#'
#' @examples
#' # Create (or replace) a Makefile for a new Bioconductor boidb extension
#' # package. The call is made while inside the package root folder:
#' biodb::ExtMakefile('.', newPkg=TRUE)$generate()
#'
#' # Create a Makefile for an existing Bioconductor package:
#' biodb::ExtMakefile('.')$generate()
#'
generate=function() {

    makefile <- self$getPath()
    if (file.exists(makefile))
        stop('Makefile "', makefile, '" already exists.')

    # Copy template
    template <- system.file('templates', 'Makefile', package='biodb')
    file.copy(template, makefile)
},

#' @description
#' Upgrades the Makefile inside the package folder. Creates one if
#' none exists.
upgrade=function() {
    
    makefile <- self$getPath()
    copy <- TRUE
    
    # Get the template makefile
    template <- system.file('templates', 'Makefile', package='biodb')
    tempver <- extractVersion(template)

    # Is there already a makefile?
    if ( ! private$overwrite && file.exists(makefile)) {

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

),

private=list(
    path=NULL,
    newPkg=NULL,
    overwrite=NULL
))
