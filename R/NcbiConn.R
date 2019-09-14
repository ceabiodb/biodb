# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiConn {{{1
################################################################################

# Declaration {{{2
################################################################################

#' NCBI connector abstract class.
#'
#' This is an abstract class, mother of all NCBI connector classes.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('ncbi.ccds')
#'
#' # Get an entry
#' e <- conn$getEntry('CCDS12227.1')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbRemotedbConn.R
#' @export NcbiConn
#' @exportClass NcbiConn
NcbiConn <- methods::setRefClass("NcbiConn",
    contains="BiodbRemotedbConn",

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    # Call parent constructor
    callSuper(...)
    .self$.abstractClass('NcbiConn')
}

))
