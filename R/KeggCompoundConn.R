# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include KeggConn.R
#' @include CompounddbConn.R
KeggCompoundConn <- methods::setRefClass("KeggCompoundConn", contains = c("KeggConn", "CompounddbConn"))

# Constructor {{{1
################################################################

KeggCompoundConn$methods( initialize = function(...) {
	callSuper(db.name = 'compound', db.abbrev = 'cpd', ...)
})

