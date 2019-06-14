# vi: fdm=marker ts=4 et cc=80

# MirbaseConn {{{1
################################################################################

#' @include BiodbRemotedbConn.R
MirbaseConn <- methods::setRefClass("MirbaseConn",
    contains="BiodbRemotedbConn",

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {
    callSuper(...)
    .self$.abstractClass('MirbaseConn')
},

# Get nb entries {{{3
################################################################################

getNbEntries=function(count=FALSE) {

    n <- NA_integer_

    ids <- .self$getEntryIds()
    if ( ! is.null(ids))
        n <- length(ids)

    return(n)
}

))
