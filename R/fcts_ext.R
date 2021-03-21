getEntryClassFile <- function(pkgRoot, dbName) {
    filename <- paste0(getEntryClassName(dbName), '.R')
    return(file.path(getFolder(pkgRoot, 'R'), filename))
}

getConnClassFile <- function(pkgRoot, dbName) {
    filename <- paste0(getConnClassName(dbName), '.R')
    return(file.path(getFolder(pkgRoot, 'R'), filename))
}

getPkgName <- function(pkgRoot) {
    name <- basename(pkgRoot)
    chk::chk_match(name, regexp="^biodb[A-Z]")
    return(name)
}
