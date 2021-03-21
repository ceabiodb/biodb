#' Extension generator abstract class
#'
#' @description
#' The mother class of all generators for biodb extension packages.
#'
#' @details
#' All generator classes for biodb extensions must inherit from this class.
#'
#' @import R6
#' @import chk
#' @export
ExtGenerator <- R6::R6Class('ExtGenerator',

public=list(
         
#' @description
#' Constructor
#' @param path      The path to the package folder.
#' @param pkgName   The package name. If set to NULL, the folder name pointer by
#' the "path" paramater will be used as the package name.
#' @param newPkg    Set to TRUE if the package is not yet published on
#' Bioconductor.
#' @param email     The email of the author.
#' @param dbName    The name of the database (in biodb format "my.db.name"),
#' that will be used in "definitions.yml" file and for connector and entry
#' classes.
#' @param dbTitle   The official name of the database (e.g.: HMDB, UniProtKB,
#' KEGG).
#' @param connType  The type of connector class to implement.
#' @param entryType The type of entry class to implement.
#' @param remote    Set to TRUE if the database to connect to is not local.
#' @param downloadable  Set to TRUE if the database needs to be downloaded or
#' offers this possiblity.
#' @param editable  Set to TRUE to allow the generated connector to create new
#' entries in memory.
#' @param writable  Set to TRUE to enable the generated connector to write into
#' the database.
#' @param rcpp      Set to TRUE to enable Rcpp C/C++ code inside the package.
#' @return A new instance.
#' @export
initialize=function(path, pkgName=NULL, email=NULL, dbName=NULL, dbTitle=NULL,
                    newPkg=FALSE, connType=c('plain', 'compound', 'mass'),
                    entryType=c('plain', 'csv', 'html', 'json', 'list', 'sdf',
                                'txt', 'xml'), editable=FALSE, writable=FALSE,
                    remote=FALSE, downloadable=FALSE, rcpp=FALSE
                    ) {
    chk::chk_string(path) # Path may not exist yet
    chk::chk_null_or(pkgName, chk::chk_match, regexp="^biodb[A-Z][A-Za-z0-9]+$")
    chk::chk_null_or(email, chk::chk_string)
    chk::chk_null_or(dbName, chk::chk_match, regexp="^[a-z0-9.]+$")
    chk::chk_null_or(dbTitle, chk::chk_string)
    chk::chk_flag(newPkg)
    chk::chk_flag(downloadable)
    chk::chk_flag(editable)
    chk::chk_flag(writable)
    chk::chk_flag(remote)
    chk::chk_flag(rcpp)
    connType <- match.arg(connType)
    entryType <- match.arg(entryType)

    private$path <- normalizePath(path, mustWork=FALSE) # Path may not exist yet
    private$pkgName <- if (is.null(pkgName)) getPkgName(private$path) else
        pkgName
    private$email <- email
    private$dbName <- dbName
    private$dbTitle <- dbTitle
    private$newPkg <- newPkg
    private$rcpp <- rcpp
    private$connType <- connType
    private$entryType <- entryType
    private$vignetteName <- 'intro'
    private$downloadable <- downloadable
    private$editable <- editable
    private$writable <- writable
    private$remote <- remote
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
    ,connType=NULL
    ,entryType=NULL
    ,vignetteName=NULL
    ,editable=NULL
    ,writable=NULL
    ,remote=NULL
    ,downloadable=NULL
))
