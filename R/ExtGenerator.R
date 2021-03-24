#' Extension generator abstract class
#'
#' @description
#' The mother class of all generators for biodb extension packages.
#'
#' @details
#' All generator classes for biodb extensions must inherit from this class.
#'
#' @examples
#' # Generate a new connector class inside R folder:
#' pkgFolder <- file.path(tempfile(), 'biodbFoo')
#' dir.create(pkgFolder, recursive=TRUE)
#' biodb::ExtConnClass$new(path=pkgFolder, dbName='foo.db',
#'                         dbTitle='Foo database',
#'                         connType='mass', remote=TRUE)$generate()
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
#' @param pkgLicense The license of the package.
#' @param newPkg    Set to TRUE if the package is not yet published on
#' Bioconductor.
#' @param email     The email of the author.
#' @param firstname     The firstname of the author.
#' @param lastname     The lastname of the author.
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
#' @param vignetteName Set to the name of the default/main vignette.
#' @param githubRepos Set to the name of the associated GitHub repository.
#' Example: myaccount/myrepos.
#' @return A new instance.
#' @export
initialize=function(path, pkgName=NULL, email=NULL, dbName=NULL, dbTitle=NULL,
                    pkgLicense=c('AGPL-3'), firstname=NULL, lastname=NULL,
                    newPkg=FALSE, connType=c('plain', 'compound', 'mass'),
                    entryType=c('plain', 'csv', 'html', 'json', 'list', 'sdf',
                                'txt', 'xml'), editable=FALSE, writable=FALSE,
                    remote=FALSE, downloadable=FALSE, rcpp=FALSE,
                    vignetteName=NULL, githubRepos=NULL
                    ) {
    chk::chk_string(path) # Path may not exist yet
    chk::chk_null_or(pkgName, chk::chk_match, regexp="^biodb[A-Z][A-Za-z0-9]+$")
    chk::chk_null_or(email, chk::chk_string)
    chk::chk_null_or(firstname, chk::chk_string)
    chk::chk_null_or(lastname, chk::chk_string)
    chk::chk_null_or(dbName, chk::chk_match, regexp="^[a-z0-9.]+$")
    chk::chk_null_or(dbTitle, chk::chk_string)
    chk::chk_null_or(vignetteName, chk::chk_string)
    chk::chk_null_or(githubRepos, chk::chk_string)
    chk::chk_flag(newPkg)
    chk::chk_flag(downloadable)
    chk::chk_flag(editable)
    chk::chk_flag(writable)
    chk::chk_flag(remote)
    chk::chk_flag(rcpp)
    connType <- match.arg(connType)
    entryType <- match.arg(entryType)
    pkgLicense <- match.arg(pkgLicense)

    private$path <- normalizePath(path, mustWork=FALSE) # Path may not exist yet
    private$pkgName <- if (is.null(pkgName)) getPkgName(private$path) else
        pkgName
    private$email <- if (is.null(email)) 'author@e.mail' else email
    private$firstname <- if (is.null(firstname)) 'Firstname of author' else
        firstname
    private$lastname <- if (is.null(lastname)) 'Lastname of author' else
        lastname
    private$dbName <- dbName
    private$dbTitle <- dbTitle
    private$newPkg <- newPkg
    private$rcpp <- rcpp
    private$connType <- connType
    private$entryType <- entryType
    private$vignetteName <- if (is.null(private$vignetteName)) 'intro' else
        vignetteName
    private$downloadable <- downloadable
    private$editable <- editable
    private$writable <- writable
    private$remote <- remote
    private$pkgLicense <- pkgLicense
    private$githubRepos <- if (is.null(githubRepos)) 'myaccount/myrepos' else
        githubRepos
}
),

private=list(
    path=NULL
    ,pkgName=NULL
    ,email=NULL
    ,firstname=NULL
    ,lastname=NULL
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
    ,pkgLicense=NULL
    ,githubRepos=NULL

,createGenerator=function(cls, ...) {
    
    # Get field values of current object
    fieldNames <- names(ExtGenerator$private_fields)
    fields <- lapply(fieldNames, function(f) private[[f]])
    names(fields) <- fieldNames
    
    # Add ellipsis
    fields <- c(fields, list(...))

    # Call constructor
    obj <- do.call(cls$new, fields)
    
    return(obj)
}
))
