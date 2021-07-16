#' Entry class for content in list format.
#'
#' This is an abstract class for handling database entries whose content is in
#' list format.
#'
#' @seealso Super class \code{\link{BiodbEntry}}.
#'
#' @examples
#' # Create a concrete entry class inheriting from CSV class:
#' MyEntry <- R6::R6Class("MyEntry", inherit=biodb::BiodbListEntry)
#'
#' @include BiodbEntry.R
#' @export
BiodbListEntry <- R6::R6Class("BiodbListEntry",
inherit=BiodbEntry,


public=list(


),

private=list(
doParseContent=function(content) {

    # Content may come from a cached file, hence as a string in JSON format, or
    # directly from memory as a list object.
    if (is.character(content))
        content <- jsonlite::fromJSON(content, simplifyDataFrame=FALSE)

    return(content)
},

doCheckParsedContent=function(parsed.content) {
    return(is.list(parsed.content) && length(parsed.content) > 0
        && ! is.null(names(parsed.content))
        && length(names(parsed.content)) > 0)
}

,doParseFieldsStep1=function(parsed.content) {

    # Get parsing expressions
    parsing.expr <- self$getParent()$getPropertyValue('parsing.expr')

    # Loop on all field names
    for (field.name in names(parsing.expr)) {

        # Is field in content?
        if (parsing.expr[[field.name]] %in% names(parsed.content)) {

            # Get value
            value <- parsed.content[[parsing.expr[[field.name]]]]

            # Skip empty vector or NA value
            if (is.vector(value) && (length(value) == 0 || all(is.na(value))))
                next

            # Set value
            self$setFieldValue(field.name, value)
        }
    }
}
))
