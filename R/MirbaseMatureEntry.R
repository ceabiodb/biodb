# vi: fdm=marker

#' @include TxtEntry.R

# class declaration {{{1
################################################################

MirbaseMatureEntry <- methods::setRefClass("MirbaseMatureEntry", contains = "TxtEntry")

# Constructor {{{1
################################################################

MirbaseMatureEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression(BIODB.ACCESSION, "^>[^ ]+ *(MIMAT[0-9]+) .*$")
	.self$addParsingExpression(BIODB.NAME, "^>([^ ]+) *MIMAT[0-9]+ .*$")
	.self$addParsingExpression(BIODB.DESCRIPTION, "^>[^ ]+ *MIMAT[0-9]+ (.*)$")
	.self$addParsingExpression(BIODB.SEQUENCE, "^([ACGU]+)$")
})
