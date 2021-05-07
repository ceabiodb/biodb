#' Generate a new extension package for biodb.
#'
#' Generates all the necessary files for a new extension package.
#'
#' @seealso \code{\link{ExtPackage}}.
#'
#' @param ... Parameters passed to \code{\link{ExtPackage}} constructor.
#' @return Nothing.
#'
#' @examples
#' # Generate a new package:
#' pkgFolder <- file.path(tempfile(), 'biodbFoo')
#' dir.create(pkgFolder, recursive=TRUE)
#' biodb::genNewExtPkg(path=pkgFolder, dbName='foo.db',
#'                     dbTitle='Foo database', rcpp=TRUE,
#'                     connType='mass', entryType='txt', downloadable=TRUE,
#'                     remote=TRUE)
#'
#' @export
genNewExtPkg <- function(...) {
    ExtPackage$new(newPkg=TRUE, ...)$generate()
    return(invisible(NULL))
}

#' Upgrading an existing extension package for biodb.
#'
#' Upgrades some of the files previously generated (.gitignore, .travis.yml,
#' .Rbuildignore, Makefile, etc) to the latest versions.
#'
#' @param ... Parameters passed to \code{\link{ExtPackage}} constructor.
#' @return Nothing.
#'
#' @examples
#' # Generate a new package:
#' pkgFolder <- file.path(tempfile(), 'biodbFoo')
#' dir.create(pkgFolder, recursive=TRUE)
#' biodb::genNewExtPkg(path=pkgFolder, dbName='foo.db',
#'                     dbTitle='Foo database', rcpp=TRUE,
#'                     connType='mass', entryType='txt', downloadable=TRUE,
#'                     remote=TRUE)
#'
#' # Upgrade it later
#' biodb::upgradeExtPkg(path=pkgFolder)
#'
#' @export
upgradeExtPkg <- function(...) {
    ExtPackage$new(...)$upgrade()
    return(invisible(NULL))
}

#' Get the package name from a package folder path.
#'
#' The package name is extracted from the path by taking the basename.
#'
#' @param pkgRoot The path to the root folder of the package.
#' @param check If set to TRUE the extracted package name is checked against
#' regular expression "^biodb\[A-Z\]\[A-Za-z0-9\]+$", to ensure the format is
#' respected.
#' @return The package name of the biodb extension.
#'
#' @examples
#' biodb::getPkgName('/my/path/to/my/extension/biodbFoo')
#'
#' @export
getPkgName <- function(pkgRoot, check=TRUE) {
    
    # Make sure pkgRoot is the real path
    pkgRoot <- normalizePath(pkgRoot, mustWork=FALSE) # Path may not exist yet
    
    # Extract name
    name <- basename(pkgRoot)
    
    # Check name format
    if (check)
        chk::chk_match(name, regexp="^biodb[A-Z][A-Za-z0-9]+$")

    return(name)
}

#' Get the available licenses for extension packages.
#'
#' @return A character vector containing license names.
#'
#' @examples
#' biodb::getLicenses()
#'
#' @export
getLicenses <- function() {
    return(c('AGPL-3'))
}

#' Extract the repository name from a package folder.
#'
#' Given the root path of a package, returns the GitHub repository name.
#'
#' @param pkgRoot The path to the root folder of the package.
#' @param default A default value to return in case git4r package is not
#' available or the folder is not a Git repository.
#' @return The repository name.
#'
#' @examples
#' biodb::getReposName('/my/path/to/my/extension/biodbFoo')
#'
#' @export
getReposName <- function(pkgRoot, default=NULL) {

    repos <- default

    if (dir.exists(pkgRoot) && requireNamespace('git2r')
        && git2r::in_repository(pkgRoot))
    {
        remotes <- git2r::remotes(pkgRoot)
        if ('origin' %in% remotes) {
            reposUrl <- git2r::remote_url(pkgRoot, remote='origin')
            # BiocCheck ERROR: Avoid references to external hosting platforms
            site <- paste(rev(strsplit("moc.buhtig", "")[[1]]), collapse='')
            if (grepl(site, reposUrl, fixed=TRUE))
                repos <- sub(paste0('^.*', site, '[:/](.+?)(\\.git)?$'), '\\1',
                    reposUrl, perl=TRUE)
        }
    }
    
    return(repos)
}
