# vi: fdm=marker

# Constants {{{1
################################################################

.BIODB.KEGG.PATHWAY.PARSING.EXPR <- list(
	'accession'              = "^ENTRY\\s+(\\S+)\\s+Pathway"
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
#' @export KeggPathwayConn
#' @exportClass KeggPathwayConn
KeggPathwayConn <- methods::setRefClass("KeggPathwayConn", contains = c("KeggConn"))

# Constructor {{{1
################################################################

KeggPathwayConn$methods( initialize = function(...) {
	callSuper(db.name = 'pathway', db.abbrev = 'path', ...)
})

# Get entry image url {{{1
################################################################

KeggPathwayConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})

# Private methods {{{1
################################################################

# Get parsing expressions {{{2
################################################################

KeggPathwayConn$methods( .getParsingExpressions = function() {
	return(.BIODB.KEGG.PATHWAY.PARSING.EXPR)
})
