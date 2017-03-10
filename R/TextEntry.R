# vi: fdm=marker

# Class declaration {{{1
################################################################

TextEntry <- methods::setRefClass("TextEntry", contains = 'BiodbEntry')


# Constructor {{{1
################################################################

TextEntry$methods( initialize = function(...) {

	superClass(...)
})
