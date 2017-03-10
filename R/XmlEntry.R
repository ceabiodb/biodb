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

# Parse content {{{1
################################################################

XmlEntry$methods( parseContent = function(content) {

	# Parse XML
	xml <-  XML::xmlInternalTreeParse(content, asText = TRUE)

	if (.self$.beforeParseContent(xml)) {

		# Test generic.parsing.expressions
		for (field in names(.self$.parsing.expr)) {
			if (is.na(.self$.xml.namespace))
				v <- XML::xpathSApply(xml, .self$.parsing.expr[[field]], XML::xmlValue)
			else
				v <- XML::xpathSApply(xml, .self$.parsing.expr[[field]], XML::xmlValue, namespaces = .self$.xml.namespace)
			if (length(v) > 0)
				.self$setFieldValue(field, v)
		}
	}

	.self$.afterParseContent(xml)
})

# Before parse content {{{1
################################################################

XmlEntry$methods( .beforeParseContent = function(xml) {
	return(TRUE)
})

# After parse content {{{1
################################################################

XmlEntry$methods( .afterParseContent = function(xml) {
})
