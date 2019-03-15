# vi: fdm=marker

#' @include BiodbTxtEntry.R

# Class declaration {{{1
################################################################

KeggCompoundEntry <- methods::setRefClass("KeggCompoundEntry", contains = 'BiodbTxtEntry')

# Constructor {{{1
################################################################

KeggCompoundEntry$methods( initialize = function(...) {

	callSuper(...)
})
