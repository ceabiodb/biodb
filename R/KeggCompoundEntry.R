# vi: fdm=marker ts=4 et cc=80 tw=80

# KeggCompoundEntry {{{1
################################################################################

# Declaration {{{2
################################################################################

#' KEGG Compound entry class.
#'
#' This is the entry class for the KEGG Compound database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('kegg.compound')
#'
#' # Get an entry
#' e <- conn$getEntry('C00133')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include KeggEntry.R
#' @export KeggCompoundEntry
#' @exportClass KeggCompoundEntry
KeggCompoundEntry <- methods::setRefClass("KeggCompoundEntry",
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

    # Parse DB links
    .self$.parseDbLinks(parsed.content)

    # Other KEGG IDs
    .self$.parseMultilinesField(field='kegg.reaction.id', tag='REACTION',
                                parsed.content=parsed.content)
    .self$.parseMultilinesField(field='kegg.enzyme.id',   tag='ENZYME',
                                parsed.content=parsed.content)
    .self$.parsePathwayIds(parsed.content=parsed.content)
}

))
