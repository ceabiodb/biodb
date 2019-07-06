# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiPubchemSubstEntry {{{1
################################################################################

#' @include NcbiPubchemEntry.R
NcbiPubchemSubstEntry <- methods::setRefClass("NcbiPubchemSubstEntry",
    contains="NcbiPubchemEntry")
