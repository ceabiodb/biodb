# vi: fdm=marker ts=4 et cc=80 tw=80

# Class declaration {{{1
################################################################################

#' NCBI connector abstract class.
#'
#' This is an abstract class, mother of all NCBI connector classes.
#'
#' @include BiodbRemotedbConn.R
#' @export NcbiConn
#' @exportClass NcbiConn
NcbiConn <- methods::setRefClass("NcbiConn", contains="BiodbRemotedbConn")

# Initialize {{{1
################################################################################

NcbiConn$methods( initialize=function(...) {

    # Call parent constructor
    callSuper(...)
    .self$.abstractClass('NcbiConn')
})
