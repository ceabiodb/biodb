# vi: fdm=marker

# Class declaration {{{1
################################################################

XmlEntry <- methods::setRefClass("XmlEntry", contains = "BiodbEntry", fields = list( .xpath.expr = 'character', .xml.namespace = "character"))

# Constructor {{{1
################################################################

XmlEntry$methods( initialize = function(xml.namespace = NA_character_, ...) {

	callSuper(...)

	.xpath.expr <<- character(0)
	.xml.namespace <<- xml.namespace
})

# Add XPath statement
################################################################

XmlEntry$methods( addXpathStatement = function(field, xpath) {

	# Check that an XPath statement has not already been defined for this field
	if (field %in% names(.self$.xpath.expr))
		.self$message(MSG.ERROR, paste("An XPath statement has already been defined for field", field))

	# Register new xpath statement
	.self$.xpath.expr[[field]] <- xpath
})

# Parse content {{{1
################################################################

XmlEntry$methods( parseContent = function(content) {

	# Parse XML
	xml <-  XML::xmlInternalTreeParse(content, asText = TRUE)

	# Test generic xpath expressions
	for (field in names(.self$.xpath.expr)) {
		if (is.na(.self$.xml.namespace))
			v <- XML::xpathSApply(xml, .self$.xpath.expr[[field]], XML::xmlValue)
		else
			v <- XML::xpathSApply(xml, .self$.xpath.expr[[field]], XML::xmlValue, namespaces = .self$.xml.namespace)
		if (length(v) > 0)
			.self$setFieldValue(field, v)
	}

	.self$runCustomXpathStatements(xml)
})

# Run custom xpath statements {{{1
################################################################

XmlEntry$methods( runCustomXpathStatements = function(xml) {
})
