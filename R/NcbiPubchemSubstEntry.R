# vi: fdm=marker ts=4 et cc=80

#' @include NcbiPubchemEntry.R

# Class declaration {{{1
################################################################################

NcbiPubchemSubstEntry <- methods::setRefClass("NcbiPubchemSubstEntry", contains="NcbiPubchemEntry")

# Initialize {{{1
################################################################################

NcbiPubchemSubstEntry$methods( initialize=function(...) {
    callSuper(...)
})
