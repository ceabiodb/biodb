# vi: fdm=marker ts=4 et cc=80 tw=80

# KeggEnzymeConn {{{1
################################################################################

#' The connector class to KEGG Enzyme database.
#'
#' This is a concrete connector class. It must never be instantiated directly,
#' but instead be instantiated through the factory \code{\link{BiodbFactory}}.
#' Only specific methods are described here. See super classes for the
#' description of inherited methods.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{KeggConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get connector
#' conn <- mybiodb$getFactory()$createConn('kegg.enzyme')
#'
#' # Get pathway IDs related to enzymes
#' pathway.ids=conn$getPathwayIds(c('1.2.1.3', '3.7.1.3'), org='mmu')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include KeggConn.R
#' @export KeggEnzymeConn
#' @exportClass KeggEnzymeConn
KeggEnzymeConn <- methods::setRefClass("KeggEnzymeConn",
    contains=c("KeggConn"),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {
    callSuper(db.name='enzyme', db.abbrev='ec', ...)
},

# Get entry image url {{{3
################################################################################

getEntryImageUrl=function(id) {
    return(rep(NA_character_, length(id)))
},

# Get pathway IDs {{{3
################################################################################

getPathwayIds=function(id, org) {
    "Get organism pathways. Given a vector of KEGG Enzyme IDs and a KEGG
    organism code, this method retrieves KEGG pathways of this organism in which
    the enzymes are involved. It returns a vector of KEGG pathway IDs."

    pathways <- character()
    fact <- .self$getBiodb()$getFactory()
    kegg.gen.conn <- fact$getConn('kegg.genes')

    # Loop on all enzymes
    for (enz.id in id) {

        pws <- NULL
        enz <- .self$getEntry(enz.id)
        if (is.null(enz))
            next

        # Does this enzyme have a list of pathways?
        if (enz$hasField('kegg.pathway.id')) {

            # Get pathways
            pws <- enz$getFieldValue('kegg.pathway.id')

            # Convert them to specified organism
            kegg.path.conn <- fact$getConn('kegg.pathway')
            pws <- kegg.path.conn$convertToOrgPathways(pws, org=org)
        }

        # Look for genes
        else if ( ! is.null(enz) && enz$hasField('kegg.genes.id')) {

            # We skip non organism genes
            genes_ids <- enz$getFieldValue('kegg.genes.id')
            mmu_genes_ids <- genes_ids[grep(paste0('^', org, ':'), genes_ids)]

            for (gene in kegg.gen.conn$getEntry(mmu_genes_ids, drop=FALSE)) {

                # We check that this gene is related to the organism:
                if ( ! is.null(gene) && gene$hasField('kegg.pathway.id')) {

                    # Get pathways
                    pws <- gene$getFieldValue('kegg.pathway.id')

                    # Filter out wrong pathways
                    kpc <- fact$getConn('kegg.pathway')
                    x <- kpc$makesRefToEntry(pws, db='kegg.enzyme',
                                             oid=enz.id, recurse=TRUE)
                    pws <- pws[x]
                }
            }
        }

        # Record found pathways
        if ( ! is.null(pws))
            pathways <- c(pathways, pws)
    }

    return(pathways)
}

))
