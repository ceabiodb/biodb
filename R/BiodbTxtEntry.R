#' Entry class for content in text format.
#'
#' This is an abstract class for handling database entries whose content is in
#' text format.
#'
#' @seealso Super class \code{\link{BiodbEntry}}.
#'
#' @examples
#' # Create a concrete entry class inheriting from CSV class:
#' MyEntry <- R6::R6Class("MyEntry", inherit=biodb::BiodbTxtEntry)
#'
#' @import R6
#' @import stringr
#' @include BiodbEntry.R
#' @export
BiodbTxtEntry <- R6::R6Class("BiodbTxtEntry",
inherit=BiodbEntry,

public=list(

#' @description
#' New instance initializer. Entry objects must not be created directly.
#' Instead, they are retrieved through the connector instances.
#' @param ... All parameters are passed to the super class initializer.
#' @return Nothing.
initialize=function(...) {

    super$initialize(...)
    abstractClass('BiodbTxtEntry', self)

    return(invisible(NULL))
}
),

private=list(
doParseContent=function(content) {

    # Get lines of content
    lines <- strsplit(content, "\r?\n")[[1]]

    return(lines)
}

,doParseFieldsStep1=function(parsed.content) {

    # Get parsing expressions
    parsing.expr <- self$getParent()$getPropertyValue('parsing.expr')

    chk::chk_character(parsed.content)
    chk::chk_list(parsing.expr)
    chk::chk_named(parsing.expr)

    # Loop on all parsing expressions
    for (field in names(parsing.expr)) {

        # Match whole content 
        g <- stringr::str_match(parsed.content, parsing.expr[[field]])

        # Get positive results
        results <- g[ ! is.na(g[, 1]), , drop=FALSE]

        # Any match ?
        if (nrow(results) > 0)
            self$setFieldValue(field, results[, 2])
    }
}
))
