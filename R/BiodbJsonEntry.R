#' Entry class for content in JSON format.
#'
#' This is an abstract class for handling database entries whose content is in
#' JSON format.
#'
#' @seealso Super class \code{\link{BiodbEntry}}.
#'
#' @examples
#' # Create a concrete entry class inheriting from CSV class:
#' MyEntry <- methods::setRefClass("MyEntry", contains="BiodbJsonEntry")
#'
#' @include BiodbEntry.R
#' @export BiodbJsonEntry
#' @exportClass BiodbJsonEntry
BiodbJsonEntry <- methods::setRefClass("BiodbJsonEntry",
    contains='BiodbEntry',

method=list(

.doParseContent=function(content) {

    # Parse JSON
    json <- jsonlite::fromJSON(content, simplifyDataFrame=FALSE)

    return(json)
},

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
