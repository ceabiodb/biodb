# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include CompounddbConn.R
#' @include RemotedbConn.R
NcbiConn <- methods::setRefClass("NcbiConn", contains = c("RemotedbConn", "CompounddbConn"))

# Constructor {{{1
################################################################

NcbiConn$methods( initialize = function(...) {

	# Call parent constructor
	callSuper(...)
	.self$.abstract.class('NcbiConn')
})
