#' Entry class for content in HTML format.
#'
#' This is an abstract class for handling database entries whose content is in
#' HTML format.
#'
#' @seealso Super class \code{\link{BiodbXmlEntry}}.
#'
#' @examples
#' # Create a concrete entry class inheriting from this class:
#' MyEntry <- R6::R6Class("MyEntry", inherit=biodb::BiodbHtmlEntry)
#'
#' @include BiodbXmlEntry.R
#' @export
BiodbHtmlEntry <- R6::R6Class("BiodbHtmlEntry",
inherit=BiodbXmlEntry,

public=list(

#' @description
#' New instance initializer. Entry objects must not be created directly.
#' Instead, they are retrieved through the connector instances.
#' @param ... All parameters are passed to the super class initializer.
#' @return Nothing.
initialize=function(...) {

    super$initialize(...)
    abstractClass('BiodbHtmlEntry', self)

    return(invisible(NULL))
}
),

private=list(
doParseContent=function(content) {

    # Parse XML
    xml <-  XML::htmlTreeParse(content, asText=TRUE, useInternalNodes=TRUE)

    return(xml)
}
))
