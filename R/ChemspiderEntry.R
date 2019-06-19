# vi: fdm=marker ts=4 et cc=80 tw=80

#' @include BiodbJsonEntry.R

# Class declaration {{{1
################################################################################

ChemspiderEntry <- methods::setRefClass("ChemspiderEntry", contains="BiodbJsonEntry")

# Initialize {{{1
################################################################################

ChemspiderEntry$methods( initialize=function(...) {

    callSuper(...)
})
