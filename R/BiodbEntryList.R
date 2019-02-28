# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include BiodbEntry.R
BiodbEntryList <- methods::setRefClass("BiodbEntryList", contains = 'BiodbEntry', fields = list())

# Private methods {{{1
################################################################

# Do parse content {{{2
################################################################

BiodbEntryList$methods( .doParseContent = function(content) {
	return(content) # Nothing to parse
})

# Is parsed content correct {{{2
################################################################

BiodbEntryList$methods( .isParsedContentCorrect = function(parsed.content) {
	return(is.list(parsed.content) && length(parsed.content) > 0 && ! is.null(names(parsed.content)) && length(names(parsed.content)) > 0)
})

# Parse fields step 1 {{{1
################################################################

BiodbEntryList$methods( .parseFieldsStep1 = function(parsed.content) {

	# Loop on all field names
	for (field.name in names(parsed.content)) {
		# Get value
		value = parsed.content[[field.name]]

		# Skip empty vector
		if (is.vector(value) && length(value) == 0)
			next

		# Set value
		.self$setFieldValue(field.name, value)
	}
})
