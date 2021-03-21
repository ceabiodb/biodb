#' Extension README file class
#'
#' @description
#' A class for generating a README file for a new extension package.
#'
#' @details
#' Write a README file inside package directory, using a template file.
#'
#' @import R6
#' @include ExtFileGenerator.R
ExtReadme <- R6::R6Class('ExtReadme',

inherit=ExtFileGenerator,

public=list(

#' @description
#' Constructor
#' @param ... See the constructor of ExtFileGenerator for the parameters.
#' @return A new instance.
initialize=function(...) {
    super$initialize(template='README.md', filename='README.md', ...)
}
),

private=list(

fillTemplate=function(templ) {

    super$fillTemplate(templ)

    # GitHub repos
    if (require(git2r) && git2r::in_repository(private$path)) {
        remotes <- git2r::remotes(private$path)
        if ('origin' %in% remotes) {
            reposUrl <- git2r::remote_url(private$path, remote='origin')
            if (grepl('github.com', reposUrl, fixed=TRUE)) {
                repos <- sub('^.*github.com[:/](.*)$', '\\1', reposUrl)
                templ$replace('githubRepos', repos)
            }
        }
    }
}
))
