# vi: fdm=marker

#' @include TxtEntry.R

# Class declaration {{{1
################################################################

ExpasyEnzymeEntry <- methods::setRefClass("ExpasyEnzymeEntry", contains = 'TxtEntry')

# Constructor {{{1
################################################################

ExpasyEnzymeEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression(BIODB.ACCESSION, "^ID\\s+([0-9.]+)$")
	.self$addParsingExpression(BIODB.NAME, "^DE\\s+(.+?)\\.?$")
	.self$addParsingExpression(BIODB.SYNONYMS, "^AN\\s+(.+?)\\.?$") # Alternate names
	.self$addParsingExpression(BIODB.CATALYTIC.ACTIVITY, "^CA\\s+(.+?)\\.?$")
	.self$addParsingExpression(BIODB.COFACTOR, "^CF\\s+(.+?)\\.?$")
})

# Parse content {{{1
################################################################

ExpasyEnzymeEntry$methods( .afterParseContent = function() {

	# Cofactors may be listed on a single line, separated by a semicolon.
	if (.self$hasField(BIODB.COFACTOR))
		.self$setFieldValue(BIODB.COFACTOR, unlist(strsplit(.self$getFieldValue(BIODB.COFACTOR), ' *; *')))
})
