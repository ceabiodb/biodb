# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include KeggConn.R
KeggCompoundConn <- methods::setRefClass("KeggCompoundConn", contains = "KeggConn")

# Constructor {{{1
################################################################

KeggCompoundConn$methods( initialize = function(...) {
	callSuper(db.name = 'compound', db.abbrev = 'cpd', ...)
})

