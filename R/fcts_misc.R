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

convertTolToRange <- function(x, tol, type=c('delta', 'plain', 'ppm')) {
    
    type <- match.arg(type)
    if (type == 'ppm') {
        a <- x - x * tol * 1e-6
        b <- x + x * tol * 1e-6
    } else { 
        a <- x - tol
        b <- x + tol
    }

    return(list(a=a, b=b))
}

extractVersion <- function(filepath) {
    chk::chk_file(filepath)
    
    version <- NULL

    # Read first lines of file
    firstLines <- readLines(filepath, n=5)
    versionLine <- grep('version[ :]', firstLines, value=TRUE)
    if (length(versionLine) > 1)
        versionLine <- versionLine[[1]]

    # Extract version number
    if (length(versionLine) == 1) {
        version <- sub('^.* version:? ([0-9]+\\.[0-9]+(\\.[0-9]+)?)$', '\\1',
                       versionLine, perl=TRUE)
        if ( ! chk::vld_match(version, regexp="^[0-9]+\\.[0-9]+(\\.[0-9]+)?$"))
            stop('Impossible to extract version number from line ("',
                 versionLine, '") of file "', filepath, '".')
    }

    return(version)
}

splitVersion <- function(version) {
    chk::chk_match(version, regexp="^[0-9]+\\.[0-9]+(\\.[0-9]+)?$")
    
    m <- stringr::str_match(version, "([0-9]+).([0-9]+)(.([0-9]+))?")
    
    return(list(major=m[1, 2], minor=m[1, 3],
                patch=(if (is.na(m[1, 4])) 0 else m[1, 5]) ))
}

compareVersions <- function(v1, v2) {
    
    cmp <- 0
    v1 <- splitVersion(v1)
    v2 <- splitVersion(v2)
    
    if (v1$major < v2$major)
        cmp <- -1
    else if (v1$major > v2$major)
        cmp <- +1
    else if (v1$minor < v2$minor)
        cmp <- -1
    else if (v1$minor > v2$minor)
        cmp <- +1
    else if (v1$patch < v2$patch)
        cmp <- -1
    else if (v1$patch > v2$patch)
        cmp <- +1
    
    return(cmp)
}
