# vi: fdm=marker

# Constants {{{1
################################################################

.BIODB.KEGG.ENZYME.PARSING.EXPR <- list(
	'accession'              = "^ENTRY\\s+EC (\\S+)\\s+Enzyme",
	'cas.id'                 = "^[DBLINKS ]+ CAS:\\s+(\\S+)$",
	'expasy.enzyme.id'       = "^[DBLINKS ]+ ExPASy - ENZYME nomenclature database:\\s+(\\S+)$"
)

# Class declaration {{{1
################################################################

#' The connector class to KEGG Enzyme database.
#'
#' This is a concrete connector class. It must never be instantiated directly, but instead be instantiated through the factory \code{\link{BiodbFactory}}. Only specific methods are described here. See super classes for the description of inherited methods.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{KeggConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get pathway IDs related to enzymes
#' pathway.ids = conn$getPathwayIds(c('1.2.1.3', '3.7.1.3'), org = 'mmu')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include KeggConn.R
#' @export KeggEnzymeConn
#' @exportClass KeggEnzymeConn
KeggEnzymeConn <- methods::setRefClass("KeggEnzymeConn", contains = c("KeggConn"))

# Constructor {{{1
################################################################

KeggEnzymeConn$methods( initialize = function(...) {
	callSuper(db.name = 'enzyme', db.abbrev = 'ec', ...)
})

# Get entry image url {{{1
################################################################

KeggEnzymeConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})

# Get pathway IDs {{{1
################################################################

KeggEnzymeConn$methods( getPathwayIds = function(id, org) {
	"Get organism pathways. Given a vector of KEGG Enzyme IDs and a KEGG organism code, this method retrieves KEGG pathways of this organism in which the enzymes are involved. It returns a vector of KEGG pathway IDs."

	pathways = character()
    kegg.gen.conn = .self$getBiodb()$getFactory()$getConn('kegg.genes')

	for (enz in .self$getEntry(id, drop = FALSE)) {

		# For each enzyme, we loop on all the genes it references:
		if ( ! is.null(enz) && enz$hasField('kegg.genes.id')) {

			# We skip non organism genes
			genes_ids = enz$getFieldValue('kegg.genes.id')
			mmu_genes_ids = genes_ids[grep(paste0('^', org, ':'), genes_ids)]

			for (gene in kegg.gen.conn$getEntry(mmu_genes_ids, drop = FALSE)) {

				# We check that this gene is related to the organism:
				if ( ! is.null(gene) && gene$hasField('kegg.organism.code') && gene$getFieldValue('kegg.organism.code') == org) {

					# We access the list of pathways to which this gene is related, and store it in our variable:
					# FIXME Here we get the list of pathways related to the gene, not to the enzyme only
					if (gene$hasField('kegg.pathway.id'))
						pathways = unique(c(pathways, gene$getFieldValue('kegg.pathway.id')))
				}
			}
		}
	}

	return(pathways)
})

# Private methods {{{1
################################################################

# Get parsing expressions {{{2
################################################################

KeggEnzymeConn$methods( .getParsingExpressions = function() {
	return(.BIODB.KEGG.ENZYME.PARSING.EXPR)
})
