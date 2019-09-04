# vi: fdm=marker ts=4 et cc=80 tw=80

# UniprotEntry {{{1
################################################################################

#' Uniprot entry class.
#'
#' @include BiodbXmlEntry.R
#' @export UniprotEntry
#' @exportClass UniprotEntry
UniprotEntry <- methods::setRefClass("UniprotEntry",
    contains="BiodbXmlEntry",

methods=list(

# Private methods {{{2
################################################################################

# Is content correct {{{1
################################################################################

.isContentCorrect=function(content) {
    return( ! grepl("^<!DOCTYPE html ", content, perl=TRUE))
},

# Parse fields step 2 {{{1
################################################################################

.parseFieldsStep2=function(parsed.content) {

    # Remove new lines from sequence string
    if (.self$hasField('aa.seq'))
        .self$setFieldValue('aa.seq',
                            gsub("\\n", "", .self$getFieldValue('aa.seq')))

    # Get synonyms
    ns <- .self$getParent()$getPropertyValue('xml.ns')
    synonyms <- XML::xpathSApply(parsed.content,
                                 "//uniprot:protein//uniprot:fullName",
                                 XML::xmlValue, namespaces=ns)
    if (length(synonyms) > 0)
        .self$appendFieldValue('name', synonyms)
}

))
