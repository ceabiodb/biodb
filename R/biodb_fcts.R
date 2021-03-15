connNameToClassPrefix <- function(connName) {
    chk::chk_string(connName)

    s <- connName
    indices <- as.integer(gregexpr('\\.[a-z]', s, perl=TRUE)[[1]])
    indices <- indices + 1  # We are interested in the letter after the dot.
    indices <- c(1, indices) # Add first letter.
    for (i in indices)
        s <- paste(substring(s, 1, i - 1), toupper(substring(s, i, i)),
                   substring(s, i + 1), sep='')
    s <- gsub('.', '', s, fixed=TRUE) # Remove dots

    return(s)
}

extractVersion <- function(filepath) {
    chk::chk_file(filepath)
    
    # Read first line of file
    firstline <- readLines(filepath, n=1)
    
    # Extract version number
    version <- sub('^.* version:? ([0-9]+\\.[0-9]+(\\.[0-9]+)?)$', '\\1',
                   firstline, perl=TRUE)
    if ( ! chk::vld_match(version, regexp="^[0-9]+\\.[0-9]+(\\.[0-9]+)?$"))
        stop('Impossible to extract version number from first line ("',
             firstline, '") of file "', filepath, '".')

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
