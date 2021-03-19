getFolder <- function(...) {
    folder <- file.path(...)
    dir.create(folder, showWarnings=FALSE, recursive=TRUE)
    return(folder)
}

getFolderFromVect <- function(path) {
    chk::chk_character(path)
    chk::chk_not_any_na(path)
    return(do.call('getFolder', as.list(path)))
}
