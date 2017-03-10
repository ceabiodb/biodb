# vi: fdm=marker

#' @include BiodbEntry.R

# Class declaration {{{1
################################################################

TxtEntry <- methods::setRefClass("TxtEntry", contains = 'BiodbEntry')


# Constructor {{{1
################################################################

TxtEntry$methods( initialize = function(...) {

	superClass(...)
})
