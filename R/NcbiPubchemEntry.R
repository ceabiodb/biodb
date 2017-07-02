# vi: fdm=marker

#' @include XmlEntry.R

# Class declaration {{{1
################################################################

NcbiPubchemEntry <- methods::setRefClass("NcbiPubchemEntry", contains = "XmlEntry")

# Constructor {{{1
################################################################

NcbiPubchemEntry$methods( initialize = function(...) {

	callSuper(...)
})

# Is parsed content correct {{{1
################################################################

NcbiPubchemEntry$methods( .isParsedContentCorrect = function(parsed.content) {
	fault <- XML::xpathSApply(parsed.content, "/Fault", XML::xmlValue)
	return(length(fault) == 0)
})
