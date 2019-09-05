# vi: fdm=marker ts=4 et cc=80 

# KeggOrthologyEntry {{{1
################################################################################

#' KEGG Orthology entry class.
#'
#' This is the class entry for KEGG Orthology database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('kegg.orthology')
#'
#' # Get an entry
#' e <- conn$getEntry('K12668')
#'
#' # Terminate instance.
#' mybiodb$terminate()
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
