# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiPubchemEntry {{{1
################################################################################

#' NCBI PubChem entry class.
#'
#' @include BiodbXmlEntry.R
#' @export NcbiPubchemEntry
#' @exportClass NcbiPubchemEntry
NcbiPubchemEntry <- methods::setRefClass("NcbiPubchemEntry",
    contains="BiodbXmlEntry",

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)
    .self$.abstractClass('NcbiPubchemEntry')
},

# Private methods {{{2
################################################################################

# Is parsed content correct {{{3
################################################################################

.isParsedContentCorrect=function(parsed.content) {
    fault <- XML::xpathSApply(parsed.content, "/Fault", XML::xmlValue)
    return(length(fault) == 0)
}

))
