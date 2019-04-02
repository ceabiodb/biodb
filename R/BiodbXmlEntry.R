# vi: fdm=marker

#' @include BiodbEntry.R

# Class declaration {{{1
################################################################

BiodbXmlEntry <- methods::setRefClass("BiodbXmlEntry", contains = "BiodbEntry", fields = list())

# Constructor {{{1
################################################################

BiodbXmlEntry$methods( initialize = function(...) {

	callSuper(...)
	.self$.abstract.class('BiodbXmlEntry')
})

# Do parse content {{{1
################################################################

BiodbXmlEntry$methods( .doParseContent = function(content) {

	xml = NULL

	# Parse XML
	if ( ! is.null(content) && is.character(content) && length(content) == 1 && ! is.na(content))
		xml <-  XML::xmlInternalTreeParse(content, asText = TRUE)

	return(xml)
})

# Parse fields step 1 {{{1
################################################################

BiodbXmlEntry$methods( .parseFieldsStep1 = function(parsed.content) {

	# Get parsing expressions
	parsing.expr <- .self$getParent()$.getParsingExpressions()

	# Set namespace
	xml.ns <- .self$getParent()$getXmlNs()
	ns <- if (is.null(xml.ns) || length(xml.ns) == 0 || all(is.na(xml.ns))) XML::xmlNamespaceDefinitions(parsed.content, simplify = TRUE) else xml.ns

	# Loop on all parsing expressions
	for (field in names(parsing.expr)) {

		# Expression using only path
		if (is.character(parsing.expr[[field]])) {

			field.single.value <- .self$getBiodb()$getEntryFields()$get(field)$hasCardOne()
			value <- NULL

			# Loop on all expressions
			for (expr in parsing.expr[[field]]) {

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
			value <- XML::xpathSApply(parsed.content, parsing.expr[[field]]$path, XML::xmlGetAttr, parsing.expr[[field]]$attr, namespaces = ns)

		# Set value
		if (length(value) > 0)
			.self$setFieldValue(field, value)
	}
})
