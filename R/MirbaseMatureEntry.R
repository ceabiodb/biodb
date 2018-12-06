# vi: fdm=marker

#' @include TxtEntry.R

# class declaration {{{1
################################################################

MirbaseMatureEntry <- methods::setRefClass("MirbaseMatureEntry", contains = "TxtEntry")

# Constructor {{{1
################################################################

MirbaseMatureEntry$methods( initialize = function(...) {

	callSuper(...)
})
