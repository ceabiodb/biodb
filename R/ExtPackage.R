#' Extension package class
#'
#' @description
#' A class for generating the skeleton of a new extension package.
#'
#' @details
#' This class manages the files of an extension package.
#'
#' It can generate all the files of a new extension package: DESCRIPTION,
#' NEWS, README.md, tests, definitons.yml, etc. Optionnaly it also generates
#' other files like: a `.travis.yml` file for Travis-CI, a Makefile for easing
#' development on UNIX-like platforms outside of Rstudio.
#'
#' It can also upgrade files of an existing package like: definitions.yml,
#' Makefile, .travis.yml, LICENSE, etc.
#'
#' @import R6
#' @import chk 
#' @export
ExtPackage <- R6::R6Class('ExtPackage',

public=list(
         
#' @description
#' Constructor
#' @param path      The path to the package folder.
#' @param pkgName   The package name. If set to NULL, the folder name pointer by
#' the "path" paramater will be used as the package name.
#' @param email     The email of the author.
#' @param dbName    The name of the database (in biodb format "my.db.name"),
#' that will be used in "definitions.yml" file and for connector and entry
#' classes.
#' @param dbTitle   The official name of the database (e.g.: HMDB, UniProtKB,
#' KEGG).
#' @param newPkg    Set to TRUE if this package is not part of Bioconductor yet.
#' @param connType  The type of connector class to implement.
#' @param entryType The type of entry class to implement.
#' @param makefile  Set to TRUE if you want a Makefile to be generated.
#' add some special directives inside the Makefile.
#' @param remote    Set to TRUE if the database to connect to is not local.
#' @param downloadable  Set to TRUE if the database needs to be downloaded or
#' offers this possiblity.
#' @param editable  Set to TRUE to allow the generated connector to create new
#' entries in memory.
#' @param writable  Set to TRUE to enable the generated connector to write into
#' the database.
#' @param rcpp      Set to TRUE to enable Rcpp C/C++ code inside the package.
#' @return A new instance.
initialize=function(path, pkgName=NULL, email=NULL, dbName=NULL, dbTitle=NULL,
                    newPkg=FALSE, connType=c('plain', 'compound', 'mass'),
                    entryType=c('plain', 'csv', 'html', 'json', 'list', 'sdf',
                                'txt', 'xml'), editable=FALSE, writable=FALSE,
                    remote=FALSE, downloadable=FALSE, rcpp=FALSE,
                    makefile=FALSE) {
    chk::chk_string(path)
    chk::chk_null_or(pkgName, chk::chk_string)
    chk::chk_null_or(email, chk::chk_string)
    chk::chk_null_or(dbName, chk::chk_string)
    chk::chk_null_or(dbTitle, chk::chk_string)
    chk::chk_flag(newPkg)
    chk::chk_flag(downloadable)
    chk::chk_flag(editable)
    chk::chk_flag(writable)
    chk::chk_flag(remote)
    chk::chk_flag(makefile)
    chk::chk_flag(rcpp)
    connType <- match.arg(connType)
    entryType <- match.arg(entryType)
    
    private$path <- normalizePath(path, mustWork=FALSE)
    private$pkgName <- if (is.null(pkgName)) getPkgName(private$path) else
        pkgName
    private$email <- email
    private$dbName <- dbName
    private$dbTitle <- dbTitle
    private$newPkg <- newPkg
    private$makefile <- makefile
    private$rcpp <- rcpp
    private$connType <- connType
    private$entryType <- entryType
    private$vignetteName <- 'intro'
    private$downloadable <- downloadable
    private$editable <- editable
    private$writable <- writable
    private$remote <- remote
}

#' @description
#' Generates the skeleton for the new extension package.
#'
#' @examples
#' # Create a new package:
#' biodb::ExtPackage$new('/path/to/my/biodbMyNewDb')$generate()
#'
,generate=function() {
    
    private$checkPathDoesNotExist()
    dir.create(private$path)

    ExtDescriptionFile$new(path=private$path, dbName=private$dbName,
                           newPkg=private$newPkg, rcpp=private$rcpp)$generate()
    if (private$makefile)
        ExtMakefile$new(path=private$path, newPkg=private$newPkg)$generate()
    ExtLicense$new(path=private$path)$generate()
    ExtReadme$new(path=private$path, dbName=private$dbName,
                  dbTitle=private$dbTitle)$generate()
    if ( ! is.null(private$dbName)) {
        ExtConnClass$new(path=private$path, dbName=private$dbName,
                         dbTitle=private$dbTitle, connType=private$connType,
                         editable=private$editable, writable=private$writable,
                         remote=private$remote,
                         downloadable=private$downloadable)$generate()
        ExtEntryClass$new(path=private$path, dbName=private$dbName,
                          dbTitle=private$dbTitle,
                          entryType=private$entryType)$generate()
        ExtDefinitions$new(path=private$path, dbName=private$dbName,
                           dbTitle=private$dbTitle, connType=private$connType,
                           entryType=private$entryType, remote=private$remote,
                           downloadable=private$downloadable)$generate()
    }
    ExtPackageFile$new(path=private$path, dbName=private$dbName,
                       rcpp=private$rcpp,
                       vignetteName=private$vignetteName)$generate()
    if (private$rcpp)
        ExtCpp$new(path=private$path)$generate()
    ExtRbuildignore$new(path=private$path)$generate()
    ExtTravisFile$new(path=private$path, pkgName=private$pkgName,
                      email=private$email)$generate()
}

#' @description
#' Upgrade files of (definitions.yml, Makefile, etc) an existing extension
#' package.
#'
#' @examples
#' # Create a new package:
#' biodb::ExtPackage$new('/path/to/my/biodbFooDb')$upgrade()
#'
,upgrade=function() {

    chk::chk_dir(private$path)
    private$checkFilesExist()

    if (private$makefile)
        ExtMakefile$new(private$path, newPkg=private$newPkg)$upgrade()
    ExtRbuildignore$new(private$path)$upgrade()
}
),

private=list(
    path=NULL
    ,pkgName=NULL
    ,email=NULL
    ,dbName=NULL
    ,dbTitle=NULL
    ,newPkg=NULL
    ,rcpp=NULL
    ,makefile=NULL
    ,connType=NULL
    ,entryType=NULL
    ,vignetteName=NULL
    ,editable=NULL
    ,writable=NULL
    ,remote=NULL
    ,downloadable=NULL

,checkFilesExist=function() {
    chk::chk_file(file.path(private$path, 'DESCRIPTION'))
    # NAMESPACE file will be generated by roxygen2.
}

,checkPathDoesNotExist=function() {
    if (dir.exists(private$path))
        stop('A folder already exists at "', private$path, '".')
    if (file.exists(private$path))
        stop('A file already exists at "', private$path, '".')
}
))
