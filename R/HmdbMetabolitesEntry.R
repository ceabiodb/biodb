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

# Parse fields step 2 {{{1
################################################################

HmdbMetabolitesEntry$methods( .parseFieldsStep2 = function(parsed.content) {

	# Remove fields with empty string
	for (f in .self$getFieldNames()) {
		v <- .self$getFieldValue(f)
		if (is.character(v) && ! is.na(v) && v == '')
			.self$removeField(f)
	}

	# Correct InChIKey
	if (.self$hasField('INCHIKEY'))
		.self$setFieldValue('INCHIKEY', sub('^InChIKey=', '', .self$getFieldValue('INCHIKEY'), perl = TRUE))

	# Synonyms
	synonyms <- XML::xpathSApply(parsed.content, "//synonym", XML::xmlValue)
	if (length(synonyms) > 0)
		.self$appendFieldValue('name', synonyms)
})
