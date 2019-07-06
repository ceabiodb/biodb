# vi: fdm=marker ts=4 et cc=80 tw=80

# UniprotEntry {{{1
################################################################################

#' @include BiodbXmlEntry.R
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
    if (.self$hasField('nt.seq'))
        .self$setFieldValue('nt.seq',
                            gsub("\\n", "", .self$getFieldValue('nt.seq')))

    # Get synonyms
    ns <- .self$getParent()$getPropertyValue('xml.ns')
    synonyms <- XML::xpathSApply(parsed.content,
                                 "//uniprot:protein//uniprot:fullName",
                                 XML::xmlValue, namespaces=ns)
    if (length(synonyms) > 0)
        .self$appendFieldValue('name', synonyms)
}

))
