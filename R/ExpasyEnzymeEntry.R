# vi: fdm=marker

#' @include TxtEntry.R

# Class declaration {{{1
################################################################

ExpasyEnzymeEntry <- methods::setRefClass("ExpasyEnzymeEntry", contains = 'TxtEntry')

# Constructor {{{1
################################################################

ExpasyEnzymeEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression('ACCESSION', "^ID\\s+([0-9.]+)$")
	.self$addParsingExpression('NAME', "^DE\\s+(.+?)\\.?$")
	.self$addParsingExpression('SYNONYMS', "^AN\\s+(.+?)\\.?$") # Alternate names
	.self$addParsingExpression('CATALYTIC.ACTIVITY', "^CA\\s+(.+?)\\.?$")
	.self$addParsingExpression('COFACTOR', "^CF\\s+(.+?)\\.?$")
})

# Parse fields after {{{1
################################################################

ExpasyEnzymeEntry$methods( .parseFieldsAfter = function(parsed.content) {

	# Cofactors may be listed on a single line, separated by a semicolon.
	if (.self$hasField('COFACTOR'))
		.self$setFieldValue('COFACTOR', unlist(strsplit(.self$getFieldValue('COFACTOR'), ' *; *')))
})
