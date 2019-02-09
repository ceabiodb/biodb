# vi: fdm=marker

#' @include XmlEntry.R

# Class declaration {{{1
################################################################

UniprotEntry <- methods::setRefClass("UniprotEntry", contains = "XmlEntry")

# Constructor {{{1
################################################################

UniprotEntry$methods( initialize = function(...) {
	callSuper(...)
})

# Is content correct {{{1
################################################################

UniprotEntry$methods( .isContentCorrect = function(content) {
	return( ! grepl("^<!DOCTYPE html ", content, perl = TRUE))
})

# Parse fields after {{{1
################################################################

UniprotEntry$methods( .parseFieldsAfter = function(parsed.content) {

	# Remove new lines from sequence string
	if (.self$hasField('SEQUENCE'))
		.self$setField('SEQUENCE', gsub("\\n", "", .self$getFieldValue('SEQUENCE')))

	# Get synonyms
	synonyms <- XML::xpathSApply(parsed.content, "//uniprot:protein//uniprot:fullName", XML::xmlValue, namespaces = .self$getParent()$getXmlNs())
	if (length(synonyms) > 0)
		.self$appendFieldValue('name', synonyms)
})
