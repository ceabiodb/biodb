# vi: fdm=marker ts=4 et cc=80

#' @include BiodbEntry.R

# Class declaration {{{1
################################################################################

BiodbTxtEntry <- methods::setRefClass("BiodbTxtEntry", contains='BiodbEntry')

# Initialize {{{1
################################################################################

BiodbTxtEntry$methods( initialize=function(...) {

    callSuper(...)
    .self$.abstractClass('BiodbTxtEntry')
})

# Do parse content {{{1
################################################################################

BiodbTxtEntry$methods( .doParseContent=function(content) {

    # Get lines of content
    lines <- strsplit(content, "\r?\n")[[1]]

    return(lines)
})

# Parse fields step 1 {{{1
################################################################################

BiodbTxtEntry$methods( .parseFieldsStep1=function(parsed.content) {

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
})
