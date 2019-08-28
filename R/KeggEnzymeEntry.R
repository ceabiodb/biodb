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
    .self$.parseNames(parsed.content)

    # Parse DB links
    .self$.parseDbLinks(parsed.content)

    # Other KEGG IDs
    .self$.parsePathwayIds(parsed.content=parsed.content)

    # Genes
    .self$.parseGenesIds(parsed.content)
}

))
