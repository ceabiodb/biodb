# vi: fdm=marker ts=4 et cc=80 tw=80

# KeggGenesEntry {{{1
################################################################################

#' @include KeggEntry.R
KeggGenesEntry <- methods::setRefClass("KeggGenesEntry",
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
    .self$.parseMultilinesField(field='name', tag='NAME',
                                parsed.content=parsed.content,
                                strip.chars=' ;', split.char=',')

    # Adjust accession with organism code
    if (.self$hasField('kegg.organism.code')) {
        org <- .self$getFieldValue('kegg.organism.code')
        acc <- .self$getFieldValue('accession')
        .self$setFieldValue('accession', paste(org, acc, sep=':'))
    }

    # Other KEGG IDs
    .self$.parseModuleIds(parsed.content)
    .self$.parsePathwayIds(parsed.content=parsed.content)

    # Parse Uniprot IDs
    if (.self$hasField('uniprot.id')) {
        ids <- strsplit(.self$getFieldValue('uniprot.id'), ' +', perl=TRUE)[[1]]
        .self$setFieldValue('uniprot.id', ids)
    }

    # AA SEQ
    lines <- .self$.getTagLines(tag='AASEQ', parsed.content=parsed.content)
    seq.length <- as.integer(lines[[1]])
    sequence <- paste(lines[2:length(lines)], collapse='')
    if (seq.length != nchar(sequence))
        .self$caution('Length of AA sequence (', nchar(sequence),
                      ') is different from the stated length (', seq.length,
                      '). In entry ', .self$getFieldValue('accession'), '.')
    .self$setFieldValue('aa.seq', sequence)
    .self$setFieldValue('aa.seq.length', seq.length)

    # NT SEQ
    lines <- .self$.getTagLines(tag='NTSEQ', parsed.content=parsed.content)
    seq.length <- as.integer(lines[[1]])
    sequence <- paste(lines[2:length(lines)], collapse='')
    if (seq.length != nchar(sequence))
        .self$caution('Length of NT sequence (', nchar(sequence),
                      ') is different from the stated length (', seq.length,
                      '). In entry ', .self$getFieldValue('accession'), '.')
    .self$setFieldValue('nt.seq', sequence)
    .self$setFieldValue('nt.seq.length', seq.length)
}

))
