# vi: fdm=marker ts=4 et cc=80 tw=80

# ExpasyEnzymeEntry {{{1
################################################################################

#' @include BiodbTxtEntry.R
ExpasyEnzymeEntry <- methods::setRefClass("ExpasyEnzymeEntry",
    contains='BiodbTxtEntry',

# Private methods {{{2
################################################################################

methods=list(

# Parse fields step 2 {{{3
################################################################################

.parseFieldsStep2=function(parsed.content) {

    # Cofactors may be listed on a single line, separated by a semicolon.
    if (.self$hasField('COFACTOR')) {
        v <- unlist(strsplit(.self$getFieldValue('COFACTOR'), ' *; *'))
        .self$setFieldValue('COFACTOR', v)
    }

    # Synonyms
    g <- stringr::str_match(parsed.content, "^AN\\s+(.+?)\\.?$")
    results <- g[ ! is.na(g[,1]), , drop=FALSE]
    if (nrow(results) > 0)
        .self$appendFieldValue('name', results[,2])
}

))
