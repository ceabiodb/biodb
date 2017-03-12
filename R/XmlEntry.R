# vi: fdm=marker

#' @include HtmlEntry.R

# Class declaration {{{1
################################################################

XmlEntry <- methods::setRefClass("XmlEntry", contains = "HtmlEntry", fields = list(.xml.namespace = "character"))

# Constructor {{{1
################################################################

XmlEntry$methods( initialize = function(xml.namespace = NA_character_, ...) {

	callSuper(...)

	.xml.namespace <<- xml.namespace
})

# Do parse content {{{1
################################################################

XmlEntry$methods( .doParseContent = function(content) {

	# Parse XML
	xml <-  XML::xmlInternalTreeParse(content, asText = TRUE)

	return(xml)
})
