# vi: fdm=marker ts=4 et cc=80

#' @include BiodbXmlEntry.R

# Class declaration {{{1
################################################################################

NcbiPubchemEntry <- methods::setRefClass("NcbiPubchemEntry", contains="BiodbXmlEntry")

# Initialize {{{1
################################################################################

NcbiPubchemEntry$methods( initialize=function(...) {

    callSuper(...)
    .self$.abstract.class('NcbiPubchemEntry')
})

# Is parsed content correct {{{1
################################################################################

NcbiPubchemEntry$methods( .isParsedContentCorrect=function(parsed.content) {
    fault <- XML::xpathSApply(parsed.content, "/Fault", XML::xmlValue)
    return(length(fault) == 0)
})
