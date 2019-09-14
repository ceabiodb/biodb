# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbListEntry {{{1
################################################################################

# Declaration {{{2
################################################################################

#' Entry class for content in list format.
#'
#' This is an abstract class for handling database entries whose content is in
#' list format.
#'
#' @include BiodbEntry.R
#' @export BiodbListEntry
#' @exportClass BiodbListEntry
BiodbListEntry <- methods::setRefClass("BiodbListEntry",
    contains='BiodbEntry',

# Private methods {{{2
################################################################################

methods=list(

# Do parse content {{{3
################################################################################

.doParseContent=function(content) {
    return(content) # Nothing to parse
},

# Is parsed content correct {{{3
################################################################################

.isParsedContentCorrect=function(parsed.content) {
    return(is.list(parsed.content) && length(parsed.content) > 0
           && ! is.null(names(parsed.content))
           && length(names(parsed.content)) > 0)
},

# Parse fields step 1 {{{3
################################################################################

.parseFieldsStep1=function(parsed.content) {

    # Loop on all field names
    for (field.name in names(parsed.content)) {

        # Get value
        value <- parsed.content[[field.name]]

        # Skip empty vector or NA value
        if (is.vector(value) && (length(value) == 0 || all(is.na(value))))
            next

        # Set value
        .self$setFieldValue(field.name, value)
    }
}

))
