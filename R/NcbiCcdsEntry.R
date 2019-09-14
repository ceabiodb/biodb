# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiCcdsEntry {{{1
################################################################################

# Declaration {{{2
################################################################################

#' NCBI CCDS entry class.
#'
#' This is the entry class for a NCBI CCDS database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('ncbi.ccds')
#'
#' # Get an entry
#' e <- conn$getEntry('CCDS12227.1')
#'
#' # Terminate instance.
#' mybiodb$terminate()
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
