# vi: fdm=marker

#' @include TxtEntry.R

# class declaration {{{1
################################################################

MirbaseMatureEntry <- methods::setRefClass("MirbaseMatureEntry", contains = "TxtEntry")

# Constructor {{{1
################################################################

MirbaseMatureEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression('ACCESSION', "^>[^ ]+ *(MIMAT[0-9]+) .*$")
	.self$addParsingExpression('NAME', "^>([^ ]+) *MIMAT[0-9]+ .*$")
	.self$addParsingExpression('DESCRIPTION', "^>[^ ]+ *MIMAT[0-9]+ (.*)$")
	.self$addParsingExpression('SEQUENCE', "^([ACGU]+)$")
})
