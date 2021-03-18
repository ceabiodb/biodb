getRFolder <- function(pkgRoot) {
    rFolder <- file.path(pkgRoot, 'R')
    dir.create(rFolder, showWarnings=FALSE, recursive=TRUE)
    return(rFolder)
}

getSrcFolder <- function(pkgRoot) {
    srcFolder <- file.path(pkgRoot, 'src')
    dir.create(srcFolder, showWarnings=FALSE, recursive=TRUE)
    return(srcFolder)
}

getEntryClassFile <- function(pkgRoot, dbName) {
    filename <- paste0(getEntryClassName(dbName), '.R')
    return(file.path(getRFolder(pkgRoot), filename))
}

getConnClassFile <- function(pkgRoot, dbName) {
    filename <- paste0(getConnClassName(dbName), '.R')
    return(file.path(getRFolder(pkgRoot), filename))
}

getPkgName <- function(pkgRoot) {
    return(basename(pkgRoot))
}
