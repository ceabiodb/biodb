# vi: fdm=marker ts=4 et cc=80 tw=80

# ChemspiderEntry {{{1
################################################################################

#' ChemSpider entry class
#'
#' @include BiodbJsonEntry.R
#' @export ChemspiderEntry
#' @exportClass ChemspiderEntry
ChemspiderEntry <- methods::setRefClass("ChemspiderEntry",
    contains="BiodbJsonEntry",

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)
}

))
