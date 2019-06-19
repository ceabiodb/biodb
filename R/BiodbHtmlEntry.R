# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbHtmlEntry {{{1
################################################################################

#' Abstract mother class for HTML entries.
#'
#' @include BiodbXmlEntry.R
BiodbHtmlEntry <- methods::setRefClass("BiodbHtmlEntry",
    contains="BiodbXmlEntry",

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)
    .self$.abstractClass('BiodbHtmlEntry')
},

# Public methods {{{2
################################################################################

# Do parse content {{{3
################################################################################

.doParseContent=function(content) {

    # Parse XML
    xml <-  XML::htmlTreeParse(content, asText=TRUE, useInternalNodes=TRUE)

    return(xml)
}

))
