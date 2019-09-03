# vi: fdm=marker ts=4 et cc=80 tw=80

# NciCactusEntry {{{1
################################################################################

#' NCI CACTUS entry class.
#'
#' @include BiodbSdfEntry.R
#' @export Explore 
#' @exportClass Explore 
NciCactusEntry <- methods::setRefClass("NciCactusEntry",
    contains="BiodbSdfEntry")
