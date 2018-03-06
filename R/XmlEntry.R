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

		# Expression using only path
		if (is.character(.self$.parsing.expr[[field]])) {

			field.single.value <- .self$getBiodb()$getEntryFields()$get(field)$hasCardOne()
			value <- NULL

			# Loop on all expressions
			for (expr in .self$.parsing.expr[[field]]) {

				# Parse
				v <- XML::xpathSApply(parsed.content, expr, XML::xmlValue, namespaces = ns)

				# The field accepts only one value
				if (field.single.value) {
					value <- v
					if (length(value) > 0)
						break
				}

				# The field accepts more than one value
				else
					value <- c(value, v)
			}
		}

		# Expression using path and attribute
		else
			value <- XML::xpathSApply(parsed.content, .self$.parsing.expr[[field]]$path, XML::xmlGetAttr, .self$.parsing.expr[[field]]$attr, namespaces = ns)

		# Set value
		if (length(value) > 0)
			.self$setFieldValue(field, value)
	}
})
