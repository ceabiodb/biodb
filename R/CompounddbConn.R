# vi: fdm=marker

#' @include BiodbConn.R

# Class declaration {{{1
################################################################

CompounddbConn <- methods::setRefClass("CompounddbConn", contains = "BiodbConn")

# Search compound {{{1
################################################################

CompounddbConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.tol = 1, mass.tol.unit = 'plain', max.results = NA_integer_) {
	.self$.abstract.method()
})
