#' Extension file generator abstract class
#'
#' @description
#' The mother class of all file generators for biodb extension packages.
#'
#' @details
#' All file generator classes for biodb extensions must inherit from this class.
#'
#' @examples
#' pkgFolder <- file.path(tempfile(), 'biodbFoo')
#' dir.create(pkgFolder, recursive=TRUE)
#' biodb::ExtConnClass$new(path=pkgFolder, dbName='foo.db',
#'                         dbTitle='Foo database',
#'                         connType='mass', remote=TRUE)$generate()
#'
#' @import R6
#' @import chk
#' @include ExtGenerator.R
#' @export
ExtFileGenerator <- R6::R6Class('ExtFileGenerator',

inherit=ExtGenerator,

public=list(
         
#' @description
#' Constructor
#' @param filename  The name of the generated file.
#' @param overwrite If set to TRUE, then overwrite existing destination file,
#' even whatever the version of the template file. If set to FALSE,
#' only overwrite if the version of the template file is strictly
#' greater than the existing destination file.
#' @param folder    The destination subfolder inside the package directory, as
#' a character vector of subfolders hierarchy.
#' @param template  The filename of the template to use.
#' @param ... See the constructor of ExtGenerator for the parameters.
#' @return A new instance.
#' @export
initialize=function(filename=NULL, overwrite=FALSE, folder=character(),
                    template=NULL, ...) {
    super$initialize(...)
    chk::chk_dir(private$path)
    chk::chk_flag(overwrite)
    chk::chk_null_or(filename, chk::chk_string) # File may not exist yet.
    chk::chk_null_or(template, chk::chk_string)
    chk::chk_character(folder)
    chk::chk_not_any_na(folder)

    private$overwrite <- overwrite
    private$template <- template
    private$folder <- folder
    private$filename <- filename
}

#' @description
#' Generates the destination file using the template file.
#' @param overwrite If set to TRUE and destination file exists, overwrite the
#' destination file.
#' @param fail If set to FALSE, do not fail if destination file exists, just do
#' nothing and return.
,generate=function(overwrite=FALSE, fail=TRUE) {
    private$generateFromTemplate(overwrite=overwrite, fail=fail)
}

#' @description
#' Try to replace remaining tags inside existing destination file.
,update=function() {
    templ <- FileTemplate$new(private$getDstFile(exist=TRUE))
    private$fillTemplate(templ)
    templ$write(private$getDstFile(exist=TRUE))
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
    filename=NULL
    ,overwrite=NULL
    ,template=NULL
    ,folder=NULL

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
    chk::chk_string(private$filename)
    return(do.call(file.path, as.list(c(private$folder, private$filename))))
}

,existsDstFile=function() {
    return(file.exists(private$buildDstPath()))
}

,buildDstPath=function() {
    chk::chk_string(private$filename)
    return(file.path(getFolderFromVect(c(private$path, private$folder)),
                     private$filename))
}

,generateFromTemplate=function(overwrite=FALSE, fail=TRUE) {

    if ( ! overwrite && private$existsDstFile()) {
        if (fail)
            stop('Cannot generate file "', private$getDstFile(),
                 '". A file of the same name already exists.')
    } else {
        templ <- FileTemplate$new(private$getTemplateFile())
        private$fillTemplate(templ)
        templ$write(private$getDstFile(), overwrite=overwrite)
    }
}

,fillTemplate=function(templ) {
    templ$replace('pkgName', private$pkgName)
    templ$replace('pkgLicense', private$pkgLicense)
    templ$replace('email', private$email)
    templ$replace('firstname', private$firstname)
    templ$replace('lastname', private$lastname)
    templ$select('new.pkg', private$newPkg)
    templ$replace('dbName', private$dbName)
    if ( ! is.null(private$dbName)) {
        templ$replace('connClass', getConnClassName(private$dbName))
        templ$replace('entryClass', getEntryClassName(private$dbName))
    }
    templ$replace('vignetteName', private$vignetteName)
    templ$select('rcpp', private$rcpp)
    templ$replace('dbTitle', private$dbTitle)
    templ$choose('conn.type', private$connType)
    templ$choose('entry.type', private$entryType)
    templ$select('remote', private$remote)
    templ$select('downloadable', private$downloadable)
    templ$select('editable', private$editable)
    templ$select('writable', private$writable)
    templ$replace('githubRepos', private$githubRepos)
}
))
