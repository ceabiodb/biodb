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
#' @return A new instance.
initialize=function(path) {
    chk::chk_dir(path)

    private$path <- normalizePath(path)
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
#' Try to replace remaining tags inside README file.
update=function() {
    temp <- FileTemplate$new(private$getReadmePath())
    private$replaceTags(temp)
    temp$write(private$getReadmePath())
}
),

private=list(
    path=NULL,

getPkgName=function() {
    return(basename(private$path))
},

getReadmePath=function() {
    return(file.path(private$path, 'README.md'))
},

replaceTags=function(template) {

    template$replace('pkgName', private$getPkgName())
    
    if (require(git2r) && git2r::in_repository(private$path)) {
        remotes <- git2r::remotes(private$path)
        if ('origin' %in% remotes) {
            reposUrl <- remote_url(private$path, remote='origin')
            if (grepl('github.com', reposUrl, fixed=TRUE)) {
                repos <- sub('^.*github.com[:/](.*)$', '\\1', reposUrl)
                template$replace('githubRepos', repos)
            }
        }
    }
}
))
