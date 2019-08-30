# vi: fdm=marker ts=4 et cc=80 tw=80

# Class declaration {{{1
################################################################################

#' Entry class for content in JSON format.
#'
#' This is an abstract class for handling database entries whose content is in
#' JSON format.
#'
#' @include BiodbEntry.R
#' @export BiodbJsonEntry
#' @exportClass BiodbJsonEntry
BiodbJsonEntry <- methods::setRefClass("BiodbJsonEntry",
    contains='BiodbEntry',

# Public methods {{{2
################################################################################

method=list(

# Initialize {{{3
################################################################################

.doParseContent=function(content) {

    # Parse JSON
    json <- jsonlite::fromJSON(content, simplifyDataFrame=FALSE)  

    return(json)
},

# Parse fields step 1 {{{3
################################################################################

.parseFieldsStep1=function(parsed.content) {

    # Get parsing expressions
    parsing.expr <- .self$getParent()$getPropertyValue('parsing.expr')

    # Set fields
    for (field in names(parsing.expr)) {

        x <- parsed.content

        # Go along path
        found.value <- TRUE
        for (t in parsing.expr[[field]])
            if (t %in% names(x))
                x <- x[[t]]
            else {
                found.value <- FALSE
                break
            }

            # Set value
        if (found.value && length(x) == 1)
            .self$setFieldValue(field, x)
    }
}

))
