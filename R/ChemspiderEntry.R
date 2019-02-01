# vi: fdm=marker

#' @include JsonEntry.R

# Class declaration {{{1
################################################################

ChemspiderEntry <- methods::setRefClass("ChemspiderEntry", contains = "JsonEntry")

# Constructor {{{1
################################################################

ChemspiderEntry$methods( initialize = function(...) {

	callSuper(...)
})
