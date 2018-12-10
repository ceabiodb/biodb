# vi: fdm=marker

#' @include XmlEntry.R

# Class declaration {{{1
################################################################

ChebiEntry <- methods::setRefClass("ChebiEntry", contains = "XmlEntry")

# Constructor {{{1
################################################################

ChebiEntry$methods( initialize = function(...) {

	callSuper(...)
})

# Parse fields after {{{1
################################################################

ChebiEntry$methods( .parseFieldsAfter = function(parsed.content) {

#	# Check that we do not have a better formula (there may be several formulae defined).
#	formulae <- XML::xpathSApply(parsed.content, "//chebi:Formulae/chebi:data", XML::xmlValue, namespaces = .self$.namespace)
#	if (length(formulae) > 1) {
#		# Try to get the ChEBI formula
#		formula <- XML::xpathSApply(parsed.content, "//chebi:Formulae/chebi:source[text()='ChEBI']/../chebi:data", XML::xmlValue, namespaces = .self$.namespace)
#		if (length(formula) > 0)
#			.self$setFieldValue('formula', formula)
#	}

	# Get synonyms
#	synonyms <- XML::xpathSApply(parsed.content, "//chebi:Synonyms/chebi:data", XML::xmlValue, namespaces = .self$.namespace)
#	if (length(synonyms) > 0)
#		.self$appendFieldValue('name', synonyms)
})
