# vi: fdm=marker ts=4 et cc=80

# Class declaration {{{1
################################################################################

#' @include BiodbRemotedbConn.R
MirbaseConn <- methods::setRefClass("MirbaseConn", contains=c("BiodbRemotedbConn"))

# Initialize {{{1
################################################################################

MirbaseConn$methods( initialize=function(...) {
    callSuper(...)
    .self$.abstractClass('MirbaseConn')
})

# Get nb entries {{{1
################################################################################

MirbaseConn$methods( getNbEntries=function(count=FALSE) {

    n <- NA_integer_

    ids <- .self$getEntryIds()
    if ( ! is.null(ids))
        n <- length(ids)

    return(n)
})
