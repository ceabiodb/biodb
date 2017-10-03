# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include RemotedbConn.R
NcbiConn <- methods::setRefClass("NcbiConn", contains = "RemotedbConn")

# Constructor {{{1
################################################################

NcbiConn$methods( initialize = function(...) {

	# Call parent constructor
	callSuper(...)
	.self$.abstract.class('NcbiConn')
})
