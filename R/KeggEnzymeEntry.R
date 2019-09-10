# vi: fdm=marker ts=4 et cc=80 tw=80

# KeggEnzymeEntry {{{1
################################################################################

#' KEGG Enzyme entry class.
#'
#' This is the entry class for the KEGG Enzyme class.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('kegg.enzyme')
#'
#' # Get an entry
#' e <- conn$getEntry('1.1.1.54')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include KeggEntry.R
#' @export KeggEnzymeEntry
#' @exportClass KeggEnzymeEntry
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
