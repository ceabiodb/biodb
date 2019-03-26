# vi: fdm=marker

# Constants {{{1
################################################################

.BIODB.KEGG.GENES.PARSING.EXPR <- list(
	'accession'              = "^ENTRY\\s+(\\S+)\\s+.*",
	'organism'               = '^ORGANISM\\s+\\S+\\s+(.*)$',
	'kegg.organism.code'     = '^ORGANISM\\s+(\\S+)\\s+.*$',
	'ncbi.gene.id'           = "^[DBLINKS ]+ NCBI-GeneID:\\s+(\\S+)$",
	'uniprot.id'             = "^[DBLINKS ]+ UniProt:\\s+(\\S+.*)$",
	'description'            = '^DEFINITION\\s+(.*)$'
)

# Class declaration {{{1
################################################################

#' The connector class to KEGG Pathway database.
#'
#' This is a concrete connector class. It must never be instantiated directly, but instead be instantiated through the factory \code{\link{BiodbFactory}}. Only specific methods are described here. See super classes for the description of inherited methods.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{KeggConn}}.
#'
#' @include KeggConn.R
#' @export KeggGenesConn
#' @exportClass KeggGenesConn
KeggGenesConn <- methods::setRefClass("KeggGenesConn", contains = c("KeggConn"))

# Constructor {{{1
################################################################

KeggGenesConn$methods( initialize = function(...) {
	callSuper(db.name = 'genes', ...)
})

# Get entry image url {{{1
################################################################

KeggGenesConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})

# Private methods {{{1
################################################################

# Get parsing expressions {{{2
################################################################

KeggGenesConn$methods( .getParsingExpressions = function() {
	return(.BIODB.KEGG.GENES.PARSING.EXPR)
})
