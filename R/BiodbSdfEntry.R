#' Entry class for content in SDF format.
#'
#' This is an abstract class for handling database entries whose content is in
#' SDF format.
#'
#' @seealso Super class \code{\link{BiodbTxtEntry}}.
#'
#' @examples
#' # Create a concrete entry class inheriting from CSV class:
#' MyEntry <- methods::setRefClass("MyEntry", contains="BiodbSdfEntry")
#'
#' @include BiodbTxtEntry.R
#' @export BiodbSdfEntry
#' @exportClass BiodbSdfEntry
BiodbSdfEntry <- methods::setRefClass("BiodbSdfEntry",
    contains='BiodbTxtEntry',
methods=list(

initialize=function(...) {

    callSuper(...)
    abstractClass('BiodbSdfEntry', .self)
},

.parseFieldsStep1=function(parsed.content) {

    # Get parsing expressions
    parsing.expr <- .self$getParent()$getPropertyValue('parsing.expr')

    chk::chk_character(parsed.content)
    chk::chk_character(parsing.expr)
    chk::chk_named(parsing.expr)

    # Tags to field
    tag2field <- character()
    for (field in names(parsing.expr)) {
        tag <- parsing.expr[[field]][[1]]
        tag2field[[tag]] <- field
    }
                        
    # Loop on lines
    tag <- NULL
    for (line in parsed.content) {

        # Remove spaces at start and end
        line <- sub('^ *', '', line)
        line <- sub(' *$', '', line)

        # Read tag
        g <- stringr::str_match(line, '^> *<(.+)>$')
        if ( ! is.na(g[1, 1]))
            tag <- g[1, 2]
        
        # Read and set value
        else if (nchar(line) > 0 && ! is.null(tag)
            && tag %in% names(tag2field)) {
            
            # Get biodb field
            field <- tag2field[[tag]]
            
            # Use regex to modify value
            if (length(parsing.expr[[field]]) > 1) {
                regex <- parsing.expr[[field]][[2]]
                line <- sub(regex, '\\1', line, perl = TRUE)
            }
            
            # Set/append value to field
            .self$appendFieldValue(field, line)
        }

        # Reset tag
        else
            tag <- NULL
    }
}

))
