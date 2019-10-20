# vi: fdm=marker ts=4 et cc=80 tw=80

# MirbaseConn {{{1
################################################################################

# Declaration {{{2
################################################################################

#' Mirbase connector class.
#'
#' This is the abstract connector class for Mirbase Mature connector class.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('mirbase.mature')
#'
#' # Get an entry
#' \dontrun{
#' e <- conn$getEntry('MIMAT0000433')
#' }
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbRemotedbConn.R
#' @export MirbaseConn
#' @exportClass MirbaseConn
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
    # Overrides super class' method.

    n <- NA_integer_

    ids <- .self$getEntryIds()
    if ( ! is.null(ids))
        n <- length(ids)

    return(n)
}

))
