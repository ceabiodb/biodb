# vi: fdm=marker

# Constants {{{1
################################################################

.BIODB.KEGG.MODULE.PARSING.EXPR <- list(
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
#' @export KeggModuleConn
#' @exportClass KeggModuleConn
KeggModuleConn <- methods::setRefClass("KeggModuleConn", contains = c("KeggConn"))

# Constructor {{{1
################################################################

KeggModuleConn$methods( initialize = function(...) {
	callSuper(db.name = 'module', db.abbrev = 'md', ...)
})

# Get entry image url {{{1
################################################################

KeggModuleConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})

# Private methods {{{1
################################################################

# Get parsing expressions {{{2
################################################################

KeggModuleConn$methods( .getParsingExpressions = function() {
	return(.BIODB.KEGG.MODULE.PARSING.EXPR)
})
