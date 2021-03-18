getRFolder <- function(pkgRoot) {
    rFolder <- file.path(pkgRoot, 'R')
    if ( ! dir.exists(rFolder))
        dir.create(rFolder)
    return(rFolder)
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
