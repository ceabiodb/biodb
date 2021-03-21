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
#' @param pkgName   The package name. If set to NULL, the folder name pointer by
#' the "path" paramater will be used as the package name.
#' @param newPkg    Set to TRUE if the package is not yet on Bioconductor.
#' @param overwrite If set to TRUE, then overwrite existing destination file,
#' even whatever the version of the template file. If set to FALSE,
#' only overwrite if the version of the template file is strictly
#' greater than the existing destination file.
#' @param template  The filename of the template to use.
#' @param folder    The destination subfolder inside the package directory, as
#' a character vector of subfolders hierarchy.
#' @param filename  The name of the generated file.
#' @param email     The email of the author.
#' @return A new instance.
initialize=function(path, filename, pkgName=NULL, newPkg=FALSE,
                    overwrite=FALSE, folder=character(), template=NULL,
                    email=NULL) {
    chk::chk_dir(path)
    chk::chk_null_or(pkgName, chk::chk_string)
    chk::chk_flag(newPkg)
    chk::chk_flag(overwrite)
    chk::chk_null_or(email, chk::chk_string)
    chk::chk_null_or(template, chk::chk_string)
    chk::chk_character(folder)
    chk::chk_not_any_na(folder)

    private$path <- normalizePath(path)
    private$pkgName <- if (is.null(pkgName)) getPkgName(private$path) else
        pkgName
    private$email <- email
    private$newPkg <- newPkg
    private$overwrite <- overwrite
    private$template <- template
    private$folder <- folder
    private$filename <- filename
}

#' @description
#' Generates the destination file using the template file.
,generate=function() {
    private$generateFromTemplate()
}

#' @description
#' Upgrades an existing destination file.
,upgrade=function() {
    
    generate <- TRUE

    # Get version of template file
    templVer <- extractVersion(private$getTemplateFile())

    # Is there already a destination file?
    if ( ! private$overwrite && private$existsDstFile()) {

        # Get version of destination file
        curVer <- extractVersion(private$getDstFile())

        # Compare versions
        cmp <- compareVersions(curVer, templVer)
        if (cmp == 0) {
            generate <- FALSE
            warning('Aborting. A local destination file "',
                    private$getDstFileRelPath(),
                    '" already exists with the same',
                    " version number (", curVer, ') than the template file "',
                    private$getTemplateFile(), '".')
        }
        else if (cmp > 0) {
            generate <- FALSE
            warning('Aborting. A local destination file "',
                    private$getDstFileRelPath(), '" already exists with a more',
                    ' recent version number (', curVer, ' > ', tempVer,
                    ') than the template file "', private$getTemplateFile(),
                    '".')
        }
    }

    # Generate or upgrade
    if (generate) {
        message("Upgrade to latest version (", templVer, ") of ",
                private$getDstFileRelPath(), '.')
        private$generateFromTemplate(overwrite=TRUE)
    }
}
),

private=list(
    path=NULL
    ,pkgName=NULL
    ,newPkg=NULL
    ,overwrite=NULL
    ,email=NULL
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

,getDstFile=function(exist=FALSE) {

    dst <- private$buildDstPath()

    if (exist)
        chk::chk_file(dst)
    else
        chk::chk_false(chk::vld_file(dst))

    return(dst)
}

,getDstFileRelPath=function() {
    return(do.call(file.path, as.list(c(private$folder, private$filename))))
}

,existsDstFile=function() {
    return(file.exists(private$buildDstPath()))
}

,buildDstPath=function() {
    return(file.path(getFolderFromVect(c(private$path, private$folder)),
                     private$filename))
}

,generateFromTemplate=function(overwrite=FALSE) {
    templ <- FileTemplate$new(private$getTemplateFile())
    templ$replace('pkgName', private$pkgName)
    templ$replace('email', private$email)
    templ$select('new.pkg', private$newPkg)
    templ$write(private$getDstFile(), overwrite=TRUE)
}
))
