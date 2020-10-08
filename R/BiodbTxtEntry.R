#' Entry class for content in text format.
#'
#' This is an abstract class for handling database entries whose content is in
#' text format.
#'
#' @seealso Super class \code{\link{BiodbEntry}}.
#'
#' @examples
#' # Create a concrete entry class inheriting from CSV class:
#' MyEntry <- methods::setRefClass("MyEntry", contains="BiodbTxtEntry")
#'
#' @import methods
#' @import stringr
#' @include BiodbEntry.R
#' @export BiodbTxtEntry
#' @exportClass BiodbTxtEntry
BiodbTxtEntry <- methods::setRefClass("BiodbTxtEntry",
    contains='BiodbEntry',

methods=list(

initialize=function(...) {

    callSuper(...)
    .self$.abstractClass('BiodbTxtEntry')
},

.doParseContent=function(content) {

    # Get lines of content
    lines <- strsplit(content, "\r?\n")[[1]]

    return(lines)
},

.parseFieldsStep1=function(parsed.content) {

    # Get parsing expressions
    parsing.expr <- .self$getParent()$getPropertyValue('parsing.expr')

    .self$.assertNotNull(parsed.content)
    .self$.assertNotNa(parsed.content)
    .self$.assertNotNull(parsing.expr)
    .self$.assertNotNa(parsing.expr)
    .self$.assertNotNull(names(parsing.expr))

    # Loop on all parsing expressions
    for (field in names(parsing.expr)) {

        # Match whole content 
        g <- stringr::str_match(parsed.content, parsing.expr[[field]])

        # Get positive results
        results <- g[ ! is.na(g[, 1]), , drop=FALSE]

        # Any match ?
        if (nrow(results) > 0)
            .self$setFieldValue(field, results[, 2])
    }
}

))
