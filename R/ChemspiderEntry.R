# vi: fdm=marker

# Class declaration {{{1
################################################################

ChemspiderEntry <- methods::setRefClass("ChemspiderEntry", contains = "XmlEntry")

# Constructor {{{1
################################################################

ChemspiderEntry$methods( initialize = function(...) {

	callSuper(...)
})
