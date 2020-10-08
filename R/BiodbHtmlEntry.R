#' Entry class for content in HTML format.
#'
#' This is an abstract class for handling database entries whose content is in
#' HTML format.
#'
#' @seealso Super class \code{\link{BiodbXmlEntry}}.
#'
#' @examples
#' # Create a concrete entry class inheriting from this class:
#' MyEntry <- methods::setRefClass("MyEntry", contains="BiodbHtmlEntry")
#'
#' @include BiodbXmlEntry.R
#' @export BiodbHtmlEntry
#' @exportClass BiodbHtmlEntry
BiodbHtmlEntry <- methods::setRefClass("BiodbHtmlEntry",
    contains="BiodbXmlEntry",

methods=list(

initialize=function(...) {

    callSuper(...)
    .self$.abstractClass('BiodbHtmlEntry')
},

.doParseContent=function(content) {

    # Parse XML
    xml <-  XML::htmlTreeParse(content, asText=TRUE, useInternalNodes=TRUE)

    return(xml)
}

))
