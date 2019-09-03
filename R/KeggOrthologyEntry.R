# vi: fdm=marker ts=4 et cc=80 

# KeggOrthologyEntry {{{1
################################################################################

#' KEGG Orthology entry class.
#'
#' @include KeggEntry.R
#' @export KeggOrthologyEntry
#' @exportClass KeggOrthologyEntry
KeggOrthologyEntry <- methods::setRefClass("KeggOrthologyEntry",
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
    .self$.parseNames(parsed.content, strip.chars=' ', split.char=',')

    # Parse DB links
    .self$.parseDbLinks(parsed.content)

    # Pathway
    .self$.parsePathwayIds(parsed.content)

    # Modules
    .self$.parseModuleIds(parsed.content)

    # Genes
    .self$.parseGenesIds(parsed.content)
}

))
