# vi: fdm=marker ts=4 et cc=80

# BiodbTxtEntry {{{1
################################################################################

#' @include BiodbEntry.R
BiodbTxtEntry <- methods::setRefClass("BiodbTxtEntry",
    contains='BiodbEntry',

# Public methods {{{2
################################################################################
methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)
    .self$.abstractClass('BiodbTxtEntry')
},

# Do parse content {{{3
################################################################################

.doParseContent=function(content) {

    # Get lines of content
    lines <- strsplit(content, "\r?\n")[[1]]

    return(lines)
},

# Parse fields step 1 {{{3
################################################################################

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
