# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiPubchemEntry {{{1
################################################################################

#' @include BiodbXmlEntry.R
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
