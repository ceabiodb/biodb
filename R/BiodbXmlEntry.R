# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbXmlEntry {{{1
################################################################################

# Declaration {{{2
################################################################################

#' Entry class for content in XML format.
#'
#' This is an abstract class for handling database entries whose content is in
#' XML format.
#'
#' @examples
#' # Create a concrete entry class inheriting from CSV class:
#' MyEntry <- methods::setRefClass("MyEntry", contains="BiodbXmlEntry")
#'
#' @import XML
#' @include BiodbEntry.R
#' @export BiodbXmlEntry
#' @exportClass BiodbXmlEntry
BiodbXmlEntry <- methods::setRefClass("BiodbXmlEntry",
    contains="BiodbEntry",

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)
    .self$.abstractClass('BiodbXmlEntry')
},

# Private methods {{{2
################################################################################

# Do parse content {{{3
################################################################################

.doParseContent=function(content) {

    xml <- NULL

    # Parse XML
    if ( ! is.null(content) && is.character(content)
        && length(content) == 1 && ! is.na(content))
        xml <-  XML::xmlInternalTreeParse(content, asText=TRUE)

    return(xml)
},

# Parse fields step 1 {{{3
################################################################################

.parseFieldsStep1=function(parsed.content) {

    ef <- .self$getBiodb()$getEntryFields()
 
    # Get parsing expressions
    parsing.expr <- .self$getParent()$getPropertyValue('parsing.expr')

    # Set namespace
    xml.ns <- .self$getParent()$getPropertyValue('xml.ns')
    ns <- if (is.null(xml.ns) || length(xml.ns) == 0 || all(is.na(xml.ns)))
        XML::xmlNamespaceDefinitions(parsed.content, simplify=TRUE) else xml.ns

    # Loop on all parsing expressions
    for (field in names(parsing.expr)) {

        # Expression using only path
        if (is.character(parsing.expr[[field]])) {

            field.single.value <- ef$get(field)$hasCardOne()
            value <- NULL

            # Loop on all expressions
            for (expr in parsing.expr[[field]]) {

                # Parse
                v <- XML::xpathSApply(parsed.content, expr, XML::xmlValue,
                                      namespaces=ns)

                # The field accepts only one value
                if (field.single.value) {
                    value <- v
                    if (length(value) > 0)
                        break
                }

                # The field accepts more than one value
                else
                    value <- c(value, v)
            }
        }

        # Expression using path and attribute
        else {
            pth <- parsing.expr[[field]]$path
            attr <- parsing.expr[[field]]$attr
            value <- XML::xpathSApply(parsed.content, pth, XML::xmlGetAttr,
                                      attr, namespaces=ns)
        }

        # Set value
        if (length(value) > 0)
            .self$setFieldValue(field, value)
    }
}

))
