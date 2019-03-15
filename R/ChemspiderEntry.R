# vi: fdm=marker

#' @include BiodbJsonEntry.R

# Class declaration {{{1
################################################################

ChemspiderEntry <- methods::setRefClass("ChemspiderEntry", contains = "BiodbJsonEntry")

# Constructor {{{1
################################################################

ChemspiderEntry$methods( initialize = function(...) {

	callSuper(...)
})
