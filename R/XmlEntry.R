# vi: fdm=marker

#' @include BiodbEntry.R

# Class declaration {{{1
################################################################

XmlEntry <- methods::setRefClass("XmlEntry", contains = "BiodbEntry", fields = list(.namespace = "character"))

# Constructor {{{1
################################################################

XmlEntry$methods( initialize = function(namespace = NA_character_, ...) {

	callSuper(...)
	.self$.abstract.class('XmlEntry')

	.namespace <<- namespace
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

	# Set namespace
	ns <- if (is.na(.self$.namespace)) XML::xmlNamespaceDefinitions(parsed.content, simplify = TRUE) else .self$.namespace

	# Loop on all parsing expressions
	for (field in names(.self$.parsing.expr)) {

		# Parse value
		if (is.character(.self$.parsing.expr[[field]]))
			v <- XML::xpathSApply(parsed.content, .self$.parsing.expr[[field]], XML::xmlValue, namespaces = ns)
		else
			v <- XML::xpathSApply(parsed.content, .self$.parsing.expr[[field]]$path, XML::xmlGetAttr, .self$.parsing.expr[[field]]$attr, namespaces = ns)

		# Set value
		if (length(v) > 0)
			.self$setFieldValue(field, v)
	}
})
