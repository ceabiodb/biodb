# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbSdfEntry {{{1
################################################################################

#' @include BiodbTxtEntry.R
BiodbSdfEntry <- methods::setRefClass("BiodbSdfEntry",
    contains='BiodbTxtEntry',

# Public methods {{{2
################################################################################
methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)
    .self$.abstractClass('BiodbSdfEntry')
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
