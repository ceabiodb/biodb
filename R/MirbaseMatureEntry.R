# vi: fdm=marker ts=4 et cc=80 tw=80

# MirbaseMatureEntry {{{1
################################################################################

#' miRBase Mature entry class.
#'
#' @include BiodbTxtEntry.R
#' @export MirbaseMatureEntry
#' @exportClass MirbaseMatureEntry
MirbaseMatureEntry <- methods::setRefClass("MirbaseMatureEntry",
    contains="BiodbTxtEntry")
