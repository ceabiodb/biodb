# vi: fdm=marker

# Class declaration {{{1
################################################################

XmlEntry <- methods::setRefClass("XmlEntry", contains = "BiodbEntry", fields = list(.xml.namespace = "character"))

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

# Parse fields from expressions {{{1
################################################################

XmlEntry$methods( .parseFieldsFromExpr = function(parsed.content) {

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
