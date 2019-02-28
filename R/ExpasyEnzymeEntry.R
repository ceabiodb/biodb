# vi: fdm=marker

#' @include TxtEntry.R

# Class declaration {{{1
################################################################

ExpasyEnzymeEntry <- methods::setRefClass("ExpasyEnzymeEntry", contains = 'TxtEntry')

# Constructor {{{1
################################################################

ExpasyEnzymeEntry$methods( initialize = function(...) {

	callSuper(...)
})

# Parse fields step 2 {{{1
################################################################

ExpasyEnzymeEntry$methods( .parseFieldsStep2 = function(parsed.content) {

	# Cofactors may be listed on a single line, separated by a semicolon.
	if (.self$hasField('COFACTOR'))
		.self$setFieldValue('COFACTOR', unlist(strsplit(.self$getFieldValue('COFACTOR'), ' *; *')))

	# Synonyms
	g <- stringr::str_match(parsed.content, "^AN\\s+(.+?)\\.?$")
	results <- g[ ! is.na(g[,1]), , drop = FALSE]
	if (nrow(results) > 0)
		.self$appendFieldValue('name', results[,2])
})
