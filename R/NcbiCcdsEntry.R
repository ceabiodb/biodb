# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiCcdsEntry {{{1
################################################################################

#' NCBI CCDS entry class.
#'
#' @include BiodbHtmlEntry.R
#' @export NcbiCcdsEntry
#' @exportClass NcbiCcdsEntry
NcbiCcdsEntry <- methods::setRefClass("NcbiCcdsEntry",
    contains="BiodbHtmlEntry",

# Private methods {{{2
################################################################################

methods=list(

# Is parsed content correct {{{3
################################################################################

.isParsedContentCorrect=function(parsed.content) {
    xpath <- "//*[starts-with(.,'No results found for CCDS ID ')]"
    return(length(XML::getNodeSet(parsed.content, xpath)) == 0)
}

))
