# vi: fdm=marker

#' @include KeggConn.R

# Class declaration {{{1
################################################################

#'KEGG Compound connection class.
#'@export
KeggCompoundConn <- methods::setRefClass("KeggCompoundConn", contains = "KeggConn")

# Constructor {{{1
################################################################

KeggCompoundConn$methods( initialize = function(...) {
	callSuper(db.name = 'compound', db.abbrev = 'cpd', ...)
})

