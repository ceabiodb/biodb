# vi: fdm=marker ts=4 et cc=80

#' @include KeggEntry.R

# Class declaration {{{1
################################################################################

KeggEnzymeEntry <- methods::setRefClass("KeggEnzymeEntry", contains = 'KeggEntry')

# Initialize {{{1
################################################################################

KeggEnzymeEntry$methods( initialize = function(...) {

    callSuper(...)
})

# Parse fields step 2 {{{1
################################################################################

KeggEnzymeEntry$methods( .parseFieldsStep2 = function(parsed.content) {

    # Name
    .self$.parseMultilinesField(field = 'name', tag = 'NAME', parsed.content = parsed.content, strip.chars = ' ;', split.char = NA_character_)

    # Other KEGG IDs
    .self$.parsePathwayIds(parsed.content = parsed.content)

    # Genes
    lines = .self$.getTagLines(tag = 'GENES', parsed.content = parsed.content)
    if (length(lines) > 0) {
        genes.ids = character()
        m = stringr::str_match(lines, "^\\s*([^:]+):\\s*(.*)\\s*$")
        org = tolower(m[, 2])
        genes = gsub('\\([^)]+\\)', '', m[, 3], perl = TRUE)
        for (i in seq_along(org))
            genes.ids = c(genes.ids, vapply(strsplit(genes[[i]], ' ')[[1]], function(gene) paste(org[[i]], gene, sep = ':'), FUN.VALUE = ''))
        .self$setFieldValue('kegg.genes.id', genes.ids)
    }
})
