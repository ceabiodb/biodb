# vi: fdm=marker

#' @include HtmlEntry.R

# Class declaration {{{1
################################################################

NcbiCcdsEntry <- methods::setRefClass("NcbiCcdsEntry", contains = "HtmlEntry")

# Constructor {{{1
################################################################

NcbiCcdsEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression('ACCESSION', list(path = "//input[@id='DATA']", attr = "value"))
	.self$addParsingExpression('SEQUENCE', "//b[starts-with(.,'Nucleotide Sequence')]/../tt")
})

# Is parsed content correct {{{1
################################################################

NcbiCcdsEntry$methods( .isParsedContentCorrect = function(parsed.content) {
	return(length(XML::getNodeSet(parsed.content, "//*[starts-with(.,'No results found for CCDS ID ')]")) == 0)
})
