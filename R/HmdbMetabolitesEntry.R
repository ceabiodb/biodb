# vi: fdm=marker

#' @include XmlEntry.R

# Class declaration {{{1
################################################################

HmdbMetabolitesEntry <- methods::setRefClass("HmdbMetabolitesEntry", contains = "XmlEntry")

# Constructor {{{1
################################################################

HmdbMetabolitesEntry$methods( initialize = function(...) {

	callSuper(...)
})

# Is parsed content correct {{{1
################################################################

HmdbMetabolitesEntry$methods( .isParsedContentCorrect = function(parsed.content) {
	return(length(XML::getNodeSet(parsed.content, "//error")) == 0)
})

# Parse fields after {{{1
################################################################

HmdbMetabolitesEntry$methods( .parseFieldsAfter = function(parsed.content) {

	# Correct InChIKey
	if (.self$hasField('INCHIKEY'))
		.self$setFieldValue('INCHIKEY', sub('^InChIKey=', '', .self$getFieldValue('INCHIKEY'), perl = TRUE))

	# Synonyms
	synonyms <- XML::xpathSApply(parsed.content, "//synonym", XML::xmlValue)
	if (length(synonyms) > 0)
		.self$appendFieldValue('name', synonyms)
})
