# vi: fdm=marker

#' @include BiodbEntry.R

# Class declaration {{{1
################################################################

HtmlEntry <- methods::setRefClass("HtmlEntry", contains = "BiodbEntry")

# Constructor {{{1
################################################################

HtmlEntry$methods( initialize = function(...) {

	callSuper(...)
})

# Do parse content {{{1
################################################################

HtmlEntry$methods( .doParseContent = function(content) {

	# Parse XML
	xml <-  XML::htmlTreeParse(content, asText = TRUE, useInternalNodes = TRUE)

	return(xml)
})

# Parse fields from expressions {{{1
################################################################

HtmlEntry$methods( .parseFieldsFromExpr = function(parsed.content) {

	# Loop on all parsing expressions
	for (field in names(.self$.parsing.expr)) {
		if (is.na(.self$.xml.namespace))
			v <- XML::xpathSApply(parsed.content, .self$.parsing.expr[[field]], XML::xmlValue)
		else
			v <- XML::xpathSApply(parsed.content, .self$.parsing.expr[[field]], XML::xmlValue, namespaces = .self$.xml.namespace)
		if (length(v) > 0)
			.self$setFieldValue(field, v)
	}
})
