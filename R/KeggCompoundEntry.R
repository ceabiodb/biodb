# vi: fdm=marker

#' @include TxtEntry.R

# Class declaration {{{1
################################################################

KeggCompoundEntry <- methods::setRefClass("KeggCompoundEntry", contains = 'TxtEntry')

# Constructor {{{1
################################################################

KeggCompoundEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression('ACCESSION', "^ENTRY\\s+(\\S+)\\s+Compound")
	.self$addParsingExpression('NAME', "^NAME\\s+([^,;]+)")
	.self$addParsingExpression('FORMULA', "^FORMULA\\s+(\\S+)$")
	.self$addParsingExpression('exact.mass', "^EXACT_MASS\\s+(\\S+)$")
	.self$addParsingExpression('MOLECULAR.WEIGHT', "^MOL_WEIGHT\\s+(\\S+)$")
})
