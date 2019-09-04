# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiPubchemSubstEntry {{{1
################################################################################

#' NCBI PubChem Substance entry class.
#'
#' @include NcbiPubchemEntry.R
#' @export NcbiPubchemSubstEntry
#' @exportClass NcbiPubchemSubstEntry
NcbiPubchemSubstEntry <- methods::setRefClass("NcbiPubchemSubstEntry",
    contains="NcbiPubchemEntry")
