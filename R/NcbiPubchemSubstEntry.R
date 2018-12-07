# vi: fdm=marker

#' @include NcbiPubchemEntry.R

# Class declaration {{{1
################################################################

NcbiPubchemSubstEntry <- methods::setRefClass("NcbiPubchemSubstEntry", contains = "NcbiPubchemEntry")

# Constructor {{{1
################################################################

NcbiPubchemSubstEntry$methods( initialize = function(...) {
	callSuper(...)
})
