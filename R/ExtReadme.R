#' Extension README file class
#'
#' @description
#' A class for generating a README file for a new extension package.
#'
#' @details
#' Write a README file inside package directory, using a template file.
#'
#' @import R6
#' @import chk
ExtReadme <- R6::R6Class('ExtReadme',

public=list(

#' @description
#' Constructor
#' @param path     The path to the package folder.
#' @param dbName   The name of the database (in biodb format "my.db.name"),
#' that will be used in "definitions.yml" file and for connector and entry
#' classe.
#' @param dbTitle  The official name of the database (e.g.: HMDB, UniProtKB,
#' KEGG).
#' @return A new instance.
initialize=function(path, dbName=NULL, dbTitle=NULL) {
    chk::chk_dir(path)
    chk::chk_null_or(dbName, chk::chk_string)
    chk::chk_null_or(dbTitle, chk::chk_string)

    private$path <- normalizePath(path)
    private$dbName <- dbName
    private$dbTitle <- dbTitle
},

#' @description
#' Generates README file.
generate=function() {
    temp <- FileTemplate$new(system.file('templates', 'README.md',
                                         package='biodb'))
    private$replaceTags(temp)
    temp$write(private$getReadmePath())
},

#' @description
#' Try to replace remaining tags inside existing README file.
update=function() {
    temp <- FileTemplate$new(private$getReadmePath())
    private$replaceTags(temp)
    temp$write(private$getReadmePath())
}
),

private=list(
    path=NULL,
    dbName=NULL,
    dbTitle=NULL,

getPkgName=function() {
    return(basename(private$path))
},

getReadmePath=function() {
    return(file.path(private$path, 'README.md'))
},

replaceTags=function(template) {

    # Package name
    template$replace('pkgName', private$getPkgName())
    
    # Database name
    if ( ! is.null(private$dbName)) {
        template$replace('dbName', private$dbName)
        template$replace('connClassName', getConnClassName(private$dbName))
    }
    
    # Database title
    if ( ! is.null(private$dbTitle))
        template$replace('dbTitle', private$dbTitle)

    # GitHub repos
    if (require(git2r) && git2r::in_repository(private$path)) {
        remotes <- git2r::remotes(private$path)
        if ('origin' %in% remotes) {
            reposUrl <- git2r::remote_url(private$path, remote='origin')
            if (grepl('github.com', reposUrl, fixed=TRUE)) {
                repos <- sub('^.*github.com[:/](.*)$', '\\1', reposUrl)
                template$replace('githubRepos', repos)
            }
        }
    }
}
))
