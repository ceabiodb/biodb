# vi: fdm=marker ts=4 et cc=80 tw=80

# ChebiEntry {{{1
################################################################################

#' ChEBI entry class.
#'
#' @include BiodbXmlEntry.R
#' @export ChebiEntry
#' @exportClass ChebiEntry
ChebiEntry <- methods::setRefClass("ChebiEntry",
    contains="BiodbXmlEntry"
)
