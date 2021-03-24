getConnClassName <- function(connName) {
    return(paste0(connNameToClassPrefix(connName), 'Conn'))
}

getEntryClassName <- function(connName) {
    return(paste0(connNameToClassPrefix(connName), 'Entry'))
}

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
