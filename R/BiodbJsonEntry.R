#' Entry class for content in JSON format.
#'
#' This is an abstract class for handling database entries whose content is in
#' JSON format.
#'
#' @seealso Super class \code{\link{BiodbEntry}}.
#'
#' @examples
#' # Create a concrete entry class inheriting from CSV class:
#' MyEntry <- R6::R6Class("MyEntry", inherit=biodb::BiodbJsonEntry)
#'
#' @include BiodbEntry.R
#' @export
BiodbJsonEntry <- R6::R6Class("BiodbJsonEntry",
inherit=BiodbEntry,


public=list(


),

private=list(
doParseContent=function(content) {

    # Parse JSON
    json <- jsonlite::fromJSON(content, simplifyDataFrame=FALSE)

    return(json)
}

,doParseFieldsStep1=function(parsed.content) {

    # Get parsing expressions
    parsing.expr <- self$getParent()$getPropertyValue('parsing.expr')

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
            self$setFieldValue(field, x)
    }
}
))
