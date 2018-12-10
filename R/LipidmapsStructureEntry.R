# vi: fdm=marker

#' @include CsvEntry.R

# Class declaration {{{1
################################################################

LipidmapsStructureEntry <- methods::setRefClass("LipidmapsStructureEntry", contains = 'CsvEntry')

# Constructor {{{1
################################################################

LipidmapsStructureEntry$methods( initialize = function(...) {

	callSuper(na.strings = c('', '-'), ...)
})

# Is content correct {{{1
################################################################

LipidmapsStructureEntry$methods( .isContentCorrect = function(content) {
	return( ! grepl("No record found", content))
})

# Parse fields after {{{1
################################################################

LipidmapsStructureEntry$methods( .parseFieldsAfter = function(parsed.content) {

	# Set synonyms 
	if (.self$hasField('SYNONYMS')) {
		v <- strsplit(.self$getFieldValue('SYNONYMS'), ';')[[1]]
		v <- sub('^ +', '', v, perl = TRUE)
		v <- sub(' +$', '', v, perl = TRUE)
		.self$setFieldValue('SYNONYMS', v)
	}

	# Synonyms
	if ('SYNONYMS' %in% names(parsed.content)) {
		v <- parsed.content[['SYNONYMS']]
		if ( ! is.na(v)) {
			v <- strsplit(v, ' *; *')[[1]]
			.self$appendFieldValue('name', v)
		}
	}
})
