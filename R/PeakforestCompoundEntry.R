# vi: fdm=marker ts=4 et cc=80 tw=80


# PeakforestCompoundEntry {{{1
################################################################################

#' @include BiodbJsonEntry.R
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
