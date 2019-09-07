# vi: fdm=marker ts=4 et cc=80 tw=80


# PeakforestCompoundEntry {{{1
################################################################################

#' PeakForest Compound entry class.
#'
#' This is the entry class for PeakForest Compound database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('peakforest.compound')
#'
#' # Get an entry
#' e <- conn$getEntry('1839')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbJsonEntry.R
#' @export PeakforestCompoundEntry
#' @exportClass PeakforestCompoundEntry
PeakforestCompoundEntry <- methods::setRefClass("PeakforestCompoundEntry",
    contains="BiodbJsonEntry",

# Private methods {{{2
################################################################################

methods=list(

# Parse fields step 2 {{{3
################################################################################

.parseFieldsStep2=function(parsed.content) {

    # HMDB null
    if (.self$hasField('hmdb.metabolites.id')) {
        v <- .self$getFieldValue('hmdb.metabolites.id')
        v <- v[v != 'HMDBnull']
        if (length(v) > 0)
            .self$setFieldValue('hmdb.metabolites.id', v)
        else
            .self$removeField('hmdb.metabolites.id')
    }

    # ChEBI IDs
    if (.self$hasField('chebi.id')) {
        v <- .self$getFieldValue('chebi.id')
        v <- sub('^CHEBI:', '', v)
        .self$setFieldValue('chebi.id', v)
    }
}

))
