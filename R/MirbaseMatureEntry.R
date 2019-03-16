# vi: fdm=marker

#' @include BiodbTxtEntry.R

# class declaration {{{1
################################################################

MirbaseMatureEntry <- methods::setRefClass("MirbaseMatureEntry", contains = "BiodbTxtEntry")

# Constructor {{{1
################################################################

MirbaseMatureEntry$methods( initialize = function(...) {

	callSuper(...)
})
