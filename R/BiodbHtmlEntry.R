# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbHtmlEntry {{{1
################################################################################

# Declaration {{{2
################################################################################

#' Entry class for content in HTML format.
#'
#' This is an abstract class for handling database entries whose content is in
#' HTML format.
#'
#' @examples
#' # Create a concrete entry class inheriting from this class:
#' MyEntry <- methods::setRefClass("MyEntry", contains="BiodbHtmlEntry"(
#'
#' @include BiodbXmlEntry.R
#' @export BiodbHtmlEntry
#' @exportClass BiodbHtmlEntry
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

# Private methods {{{2
################################################################################

# Do parse content {{{3
################################################################################

.doParseContent=function(content) {

    # Parse XML
    xml <-  XML::htmlTreeParse(content, asText=TRUE, useInternalNodes=TRUE)

    return(xml)
}

))
