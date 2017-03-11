# vi: fdm=marker

#' @include TxtEntry.R

# Class declaration {{{1
################################################################

KeggCompoundEntry <- methods::setRefClass("KeggCompoundEntry", contains = 'TxtEntry')

# Constructor {{{1
################################################################

KeggCompoundEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression(BIODB.NAME, "^NAME\\s+([^,;]+)")
	.self$addParsingExpression(BIODB.ACCESSION, "^ENTRY\\s+(\\S+)\\s+Compound")
})
