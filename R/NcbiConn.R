# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include BiodbRemotedbConn.R
NcbiConn <- methods::setRefClass("NcbiConn", contains = "BiodbRemotedbConn")

# Constructor {{{1
################################################################

NcbiConn$methods( initialize = function(...) {

	# Call parent constructor
	callSuper(...)
	.self$.abstract.class('NcbiConn')
})
