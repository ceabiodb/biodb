# vi: fdm=marker

#' @include BiodbXmlEntry.R

# Class declaration {{{1
################################################################

UniprotEntry <- methods::setRefClass("UniprotEntry", contains = "BiodbXmlEntry")

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

# Parse fields step 2 {{{1
################################################################

UniprotEntry$methods( .parseFieldsStep2 = function(parsed.content) {

	# Remove new lines from sequence string
	if (.self$hasField('nt.seq'))
		.self$setField('nt.seq', gsub("\\n", "", .self$getFieldValue('nt.seq')))

	# Get synonyms
	synonyms <- XML::xpathSApply(parsed.content, "//uniprot:protein//uniprot:fullName", XML::xmlValue, namespaces = .self$getParent()$getXmlNs())
	if (length(synonyms) > 0)
		.self$appendFieldValue('name', synonyms)
})
