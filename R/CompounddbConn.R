# vi: fdm=marker

# Class declaration {{{1
################################################################

CompounddbConn <- methods::setRefClass("CompounddbConn", contains = "BiodbConn")

# Search entry by mass {{{1
################################################################

CompounddbConn$methods( searchEntryByMass = function(mass, tol, max.results = NA_integer_) {
	.self$.abstract.method()
})
