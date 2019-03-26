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

# Private methods {{{1
################################################################

# Get parsing expressions {{{2
################################################################

KeggEnzymeConn$methods( .getParsingExpressions = function() {
	return(.BIODB.KEGG.ENZYME.PARSING.EXPR)
})
