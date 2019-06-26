# vi: fdm=marker ts=4 et cc=80 tw=80

# KeggEnzymeEntry {{{1
################################################################################

#' @include KeggEntry.R
KeggEnzymeEntry <- methods::setRefClass("KeggEnzymeEntry",
    contains='KeggEntry',

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)
},

# Parse fields step 2 {{{3
################################################################################

.parseFieldsStep2=function(parsed.content) {

    # Name
    .self$.parseMultilinesField(field='name', tag='NAME',
                                parsed.content=parsed.content, strip.chars=' ;',
                                split.char=NA_character_)

    # Other KEGG IDs
    .self$.parsePathwayIds(parsed.content=parsed.content)

    # Genes
    lines <- .self$.getTagLines(tag='GENES', parsed.content=parsed.content)
    if (length(lines) > 0) {
        genes.ids <- character()
        m <- stringr::str_match(lines, "^\\s*([^:]+):\\s*(.*)\\s*$")
        org <- tolower(m[, 2])
        genes <- gsub('\\([^)]+\\)', '', m[, 3], perl=TRUE)
        for (i in seq_along(org)) {
            ids <- strsplit(genes[[i]], ' ')[[1]]
            fct <- function(gene) paste(org[[i]], gene, sep=':')
            genes.ids <- c(genes.ids, vapply(ids, fct, FUN.VALUE=''))
        }
        .self$setFieldValue('kegg.genes.id', genes.ids)
    }
}

))
