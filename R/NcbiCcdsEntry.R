# vi: fdm=marker ts=4 et cc=80 tw=80

#' @include BiodbHtmlEntry.R

# Class declaration {{{1
################################################################################

NcbiCcdsEntry <- methods::setRefClass("NcbiCcdsEntry", contains="BiodbHtmlEntry")

# Initialize {{{1
################################################################################

NcbiCcdsEntry$methods( initialize=function(...) {

    callSuper(...)
})

# Is parsed content correct {{{1
################################################################################

NcbiCcdsEntry$methods( .isParsedContentCorrect=function(parsed.content) {
    return(length(XML::getNodeSet(parsed.content, "//*[starts-with(.,'No results found for CCDS ID ')]")) == 0)
})
