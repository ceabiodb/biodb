# vi: fdm=marker ts=4 et cc=80 tw=80

# KeggReactionEntry {{{1
################################################################################

#' @include KeggEntry.R
KeggReactionEntry <- methods::setRefClass("KeggReactionEntry",
    contains='KeggEntry',

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)
},

# Private methods {{{2
################################################################################

# Parse fields step 2 {{{3
################################################################################

.parseFieldsStep2=function(parsed.content) {

    # Name
    .self$.parseNames(parsed.content)

    # Other KEGG IDs
    .self$.parseMultilinesField(field='kegg.enzyme.id', tag='ENZYME',
                                parsed.content=parsed.content)
    .self$.parsePathwayIds(parsed.content=parsed.content)
    .self$.parseModuleIds(parsed.content)

    # Parse subtrates and products
    if (.self$hasField('equation')) {
        s <- gsub(' ', '', .self$getFieldValue('equation')) # Remove spaces
        s <- strsplit(strsplit(s, '<=>')[[1]], '\\+')
        if (length(s) == 2) {
            .self$setFieldValue('substrates', s[[1]])
            .self$setFieldValue('products', s[[2]])
        }
        else
            .self$caution('Unable to parse equation "',
                          .self$getFieldValue('equation'),
                          '" of KEGG reaction ',
                          .self$getFieldValue('accession'), '.')
    }
}

))
