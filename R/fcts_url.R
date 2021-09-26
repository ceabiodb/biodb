#' Get URL content using RCurl::getURL().
#'
#' @param u The URL as a character value.
#' @param useragent The user agent identification.
#' @return The URL content as a character single value.
getRCurlContent <- function(u, useragent=NULL) {
    chk::chk_string(u)
    chk::chk_null_or(u, chk::chk_string)
}

#' Get URL content using base::url().
#'
#' @param u The URL as a character value.
#' @return The URL content as a character single value.
getBaseUrlContent <- function(u) {
    chk::chk_string(u)

    # Open URL and get URL descriptor
    ud <- url(u)

    # Get content
    content <- paste(readLines(ud), sep="\n")

    # Close URL descriptor
    close(ud)

    return(content)
}
