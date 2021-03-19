getEntryClassFile <- function(pkgRoot, dbName) {
    filename <- paste0(getEntryClassName(dbName), '.R')
    return(file.path(getFolder(pkgRoot, 'R'), filename))
}

getConnClassFile <- function(pkgRoot, dbName) {
    filename <- paste0(getConnClassName(dbName), '.R')
    return(file.path(getFolder(pkgRoot, 'R'), filename))
}

getPkgName <- function(pkgRoot) {
    return(basename(pkgRoot))
}
