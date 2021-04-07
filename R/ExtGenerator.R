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
#' @param loadCfg   Set to FALSE to disable loading of tag values from config
#' file "biodb_ext.yml".
#' @param saveCfg   Set to FALSE to disable saving of tag values into config
#' file "biodb_ext.yml".
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
#' @param makefile  Set to TRUE if you want a Makefile to be generated.
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
initialize=function(path, loadCfg=TRUE, saveCfg=TRUE, pkgName=getPkgName(path),
                    email='author@e.mail', dbName='foo.db',
                    dbTitle='Foo database',
                    pkgLicense=getLicenses(), firstname='Firstname of author',
                    lastname='Lastname of author', newPkg=FALSE,
                    connType=getConnTypes(), entryType=getEntryTypes() ,
                    editable=FALSE, writable=FALSE, remote=FALSE,
                    downloadable=FALSE, makefile=FALSE,
                    rcpp=FALSE, vignetteName='intro',
                    githubRepos=getReposName(path, default='myaccount/myrepos')
                    ) {
    allParams <- as.list(environment())
    explicitParams <- as.list(match.call())
    chk::chk_string(path) # Path may not exist yet
    private$path <- normalizePath(path, mustWork=FALSE) # Path may not exist yet
    chk::chk_flag(loadCfg)
    chk::chk_flag(saveCfg)
    nonTags <- c('path', 'loadCfg', 'saveCfg')
    
    # Load config
    tags <- if (loadCfg) private$loadConfig() else list()

    # Explicit parameters overwrite config
    explicitParams[[1]] <- NULL # Remove function name
    explicitParams[nonTags] <- NULL # Remove non-tag parameters
    tags[names(explicitParams)] <- allParams[names(explicitParams)]
    
    # Set fct default values
    allParams[nonTags] <- NULL # Remove non-tag parameters
    nonNullTags <- Filter(function(t) { return(! is.null(t)) }, tags)
    allParams[names(allParams) %in% names(nonNullTags)] <- NULL
    tags[names(allParams)] <- allParams

    # Set tags
    private$tags <- tags

    # Check tags
    private$checkTags()
    
    # Save tags into config file
    if (saveCfg)
        private$saveConfig()
}

#' @description
#' Generates the destination file(s).
#' @param overwrite If set to TRUE and destination files exist, overwrite the
#' destination files.
#' @param fail If set to FALSE, do not fail if destination files exist, just do
#' nothing and return.
#' @examples
#' # Generate a new extension package:
#' biodb::ExtPackage$new('/my/path/to/my/biodbExtension')$generate()
,generate=function(overwrite=FALSE, fail=TRUE) {
    private$doGenerate(overwrite=overwrite, fail=fail)
}
),

private=list(
    path=NULL
    ,loadCfg=NULL
    ,tags=NULL

,doGenerate=function(overwrite=FALSE, fail=TRUE) {
    stop("Abstract method doGenerate() not implemented inside concrete class.")
}

,getCfgFile=function() {
    return(file.path(private$path, "biodb_ext.yml"))
}

,loadConfig=function() {

    tags <- list()

    cfgFile <- private$getCfgFile()
    if (file.exists(cfgFile))
        tags <- yaml::read_yaml(cfgFile)

    return(tags)
}

,saveConfig=function() {
    if ( ! dir.exists(private$path))
        dir.create(private$path, recursive=TRUE)
    yaml::write_yaml(private$tags, private$getCfgFile())
}

,checkTags=function() {
    chk::chk_match(private$tags$pkgName, regexp="^biodb[A-Z][A-Za-z0-9]+$")
    chk::chk_match(private$tags$email,
                   regexp="^[a-zA-Z0-9._-]+@[a-zA-Z0-9._-]+$")
    chk::chk_string(private$tags$firstname)
    chk::chk_string(private$tags$lastname)
    chk::chk_match(private$tags$dbName, regexp="^[a-z0-9.]+$")
    chk::chk_string(private$tags$dbTitle)
    chk::chk_string(private$tags$vignetteName)
    chk::chk_match(private$tags$githubRepos,
                   regexp="^[a-zA-Z0-9_-]+/[a-zA-Z0-9_-]+$")
    chk::chk_flag(private$tags$newPkg)
    chk::chk_flag(private$tags$makefile)
    chk::chk_flag(private$tags$downloadable)
    chk::chk_flag(private$tags$editable)
    chk::chk_flag(private$tags$writable)
    chk::chk_flag(private$tags$remote)
    chk::chk_flag(private$tags$rcpp)
    private$tags$connType <- match.arg(private$tags$connType, getConnTypes())
    private$tags$entryType <- match.arg(private$tags$entryType, getEntryTypes())
    private$tags$pkgLicense <- match.arg(private$tags$pkgLicense, getLicenses())
}

,createGenerator=function(cls, ...) {

    # Add ellipsis
    fields <- c(path=private$path, private$tags, list(...), loadCfg=FALSE,
                saveCfg=FALSE)

    # Call constructor
    obj <- do.call(cls$new, fields)

    return(obj)
}
))
