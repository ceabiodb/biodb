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
#' Initializer.
#' @param filename  The name of the generated file.
#' @param overwrite If set to TRUE, then overwrite existing destination file,
#' even whatever the version of the template file. If set to FALSE,
#' only overwrite if the version of the template file is strictly
#' greater than the existing destination file.
#' @param folder    The destination subfolder inside the package directory, as
#' a character vector of subfolders hierarchy.
#' @param template  The filename of the template to use.
#' @param upgrader The type of upgrader to use. "fullReplacer" replaces
#' the whole destination file by the template if it is newer (it compares
#' version numbers). "lineAdder" only adds to the destination file the
#' missing lines from the template file.
#' @param ... See the constructor of ExtGenerator for the parameters.
#' @return Nothing.
#' @export
initialize=function(filename=NULL, overwrite=FALSE, folder=character(),
    template=NULL, upgrader=c('fullReplacer', 'lineAdder'), ...) {
    super$initialize(...)
    chk::chk_dir(private$path)
    chk::chk_flag(overwrite)
    chk::chk_null_or(filename, vld=chk::vld_string) # File may not exist yet.
    chk::chk_null_or(template, vld=chk::vld_string)
    chk::chk_character(folder)
    chk::chk_not_any_na(folder)
    upgrader <- match.arg(upgrader)

    private$overwrite <- overwrite
    private$template <- template
    private$folder <- folder
    private$filename <- filename
    private$upgrader <- upgrader

    return(invisible(NULL))
}
),

private=list(
    filename=NULL
    ,overwrite=NULL
    ,template=NULL
    ,folder=NULL
    ,upgrader=NULL

,doGenerate=function(overwrite=FALSE, fail=TRUE) {
    private$generateFromTemplate(overwrite=overwrite, fail=fail)

    return(invisible(NULL))
}

,doUpgrade=function(generate=TRUE) {

    # Call the right upgrader
    private[[private$upgrader]](generate=generate)

    return(invisible(NULL))
}

,fullReplacer=function(generate=TRUE) {

    # Get version of template file
    templVer <- extractVersion(private$getTemplateFile())

    # Is there already a destination file?
    upgradeDst <- FALSE
    if ( ! is.null(templVer) && private$existsDstFile()) {

        # Get version of destination file
        curVer <- extractVersion(private$getDstFile())
        if ( ! is.null(curVer)) {

            # Compare versions
            cmp <- compareVersions(curVer, templVer)
            #if (cmp == 0) {
            #    upgradeDst <- FALSE
            #    warning('Aborting. A local destination file "',
            #            private$getDstFileRelPath(),
            #            '" already exists with the same',
            #            " version number (", curVer,
            #            ') than the template file "',
            #            private$getTemplateFile(), '".')
            #}
            if (cmp > 0) {
                upgradeDst <- FALSE
                warn(paste('Aborting. A local destination file "%s" already',
                    ' exists with a more recent version number',
                    '(%s > %s) than the template file "%s".'),
                    private$getDstFileRelPath(), curVer, templVer,
                    private$getTemplateFile())
            }
            else
                upgradeDst <- TRUE
        }
    }

    # Generate or upgrade
    if ( ( ! private$existsDstFile() && generate) || upgradeDst) {
        logInfo0("Upgrade to latest version (", templVer, ") of ",
            private$getDstFileRelPath(), '.')
        private$generateFromTemplate(overwrite=TRUE)
    }

    return(invisible(NULL))
}

,lineAdder=function(generate=TRUE) {

    
    # Upgrade
    if (private$existsDstFile()) {
    
        dst <- private$getDstFile()

        # Read lines from templates and destination file
        templ <- FileTemplate$new(private$getTemplateFile())
        private$fillTemplate(templ)
        templLines <- templ$getLines()
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
    
    # Generate
    else if (generate) {
        self$generate()
    }

    return(invisible(NULL))
}

,update=function() {
    templ <- FileTemplate$new(private$getDstFile(exist=TRUE))
    private$fillTemplate(templ)
    templ$write(private$getDstFile(exist=TRUE))

    return(invisible(NULL))
}

,getTemplateFile=function() {
    
    templFile <- NULL

    if ( ! is.null(private$template))
        templFile <- system.file('templates', private$template,
            package='biodb', mustWork=TRUE)
    
    return(templFile)
}

,getDstFile=function(exist=NULL) {

    chk::chk_string(private$filename)
    dst <- file.path(getFolderFromVect(c(private$path, private$folder)),
        private$filename)

    if ( ! is.null(exist)) {
        if (exist)
            chk::chk_file(dst)
        else
            chk::chk_false(chk::vld_file(dst))
    }

    return(dst)
}

,getDstFileRelPath=function() {
    chk::chk_string(private$filename)
    return(do.call(file.path, as.list(c(private$folder, private$filename))))
}

,existsDstFile=function() {
    return(file.exists(private$getDstFile()))
}

,generateFromTemplate=function(overwrite=FALSE, fail=TRUE) {

    if ( ! overwrite && private$existsDstFile()) {
        if (fail)
            error('Cannot generate file "', private$getDstFile(),
                '". A file of the same name already exists.')
    } else {
        templ <- FileTemplate$new(private$getTemplateFile())
        private$fillTemplate(templ)
        templ$write(private$getDstFile(), overwrite=overwrite)
    }

    return(invisible(NULL))
}

,fillTemplate=function(templ) {

    # Loop on all tags
    for (tag in names(private$tags)) {
        if (is.logical(private$tags[[tag]]))
            templ$select(tag, private$tags[[tag]])
        else if (tag %in% c('connType', 'entryType'))
            templ$choose(tag, private$tags[[tag]])
        else
            templ$replace(tag, private$tags[[tag]])
    }
    
    # Deduced tags
    if ( ! is.null(private$tags$dbName)) {
        templ$replace('connClass', getConnClassName(private$tags$dbName))
        templ$replace('entryClass', getEntryClassName(private$tags$dbName))
    }

    return(invisible(NULL))
}
))
