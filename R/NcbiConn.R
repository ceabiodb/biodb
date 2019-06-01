# vi: fdm=marker ts=4 et cc=80

# Class declaration {{{1
################################################################################

#' @include BiodbRemotedbConn.R
NcbiConn <- methods::setRefClass("NcbiConn", contains = "BiodbRemotedbConn")

# Initialize {{{1
################################################################################

NcbiConn$methods( initialize = function(...) {

    # Call parent constructor
    callSuper(...)
    .self$.abstract.class('NcbiConn')
})
