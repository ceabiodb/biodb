# vi: fdm=marker

#' @include BiodbXmlEntry.R

# Class declaration {{{1
################################################################

BiodbHtmlEntry <- methods::setRefClass("BiodbHtmlEntry", contains = "BiodbXmlEntry")

# Constructor {{{1
################################################################

BiodbHtmlEntry$methods( initialize = function(...) {

	callSuper(...)
	.self$.abstract.class('BiodbHtmlEntry')
})

# Do parse content {{{1
################################################################

BiodbHtmlEntry$methods( .doParseContent = function(content) {

	# Parse XML
	xml <-  XML::htmlTreeParse(content, asText = TRUE, useInternalNodes = TRUE)

	return(xml)
})
