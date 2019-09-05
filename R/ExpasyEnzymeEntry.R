# vi: fdm=marker ts=4 et cc=80 tw=80

# ExpasyEnzymeEntry {{{1
################################################################################

#' Expasy Enzyme entry class.
#'
#' This is the entry class for the Expasy Enzyme database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('expasy.enzyme')
#'
#' # Get an entry
#' e <- conn$getEntry('1.1.1.1')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbTxtEntry.R
#' @export ExpasyEnzymeEntry
#' @exportClass ExpasyEnzymeEntry
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
