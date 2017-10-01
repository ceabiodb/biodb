# vi: fdm=marker

#' @include XmlEntry.R

# Class declaration {{{1
################################################################

HtmlEntry <- methods::setRefClass("HtmlEntry", contains = "XmlEntry")

# Constructor {{{1
################################################################

HtmlEntry$methods( initialize = function(...) {

	callSuper(...)
	.self$.abstract.class('HtmlEntry')
})

# Do parse content {{{1
################################################################

HtmlEntry$methods( .doParseContent = function(content) {

	# Parse XML
	xml <-  XML::htmlTreeParse(content, asText = TRUE, useInternalNodes = TRUE)

	return(xml)
})
