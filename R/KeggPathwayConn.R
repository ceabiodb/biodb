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
#' @seealso \code{\link{BiodbFactory}}, \code{\link{KeggConn}}, \code{\link{BiodbCompoundbConn}}.
#'
#' @include KeggConn.R
#' @include BiodbCompounddbConn.R
#' @export KeggPathwayConn
#' @exportClass KeggPathwayConn
KeggPathwayConn <- methods::setRefClass("KeggPathwayConn", contains = c("KeggConn", "BiodbCompounddbConn"))

# Constructor {{{1
################################################################

KeggPathwayConn$methods( initialize = function(...) {
	callSuper(db.name = 'pathway', db.abbrev = 'path', ...)
})

# Search compound {{{1
################################################################

KeggPathwayConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.field = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {
		
	.self$.checkMassField(mass = mass, mass.field = mass.field)

	ids <- NULL

	# Search by name
	if ( ! is.null(name) && ! is.na(name)) {
		ids <- .self$ws.find(name, retfmt = 'ids')
		ids <- sub('^path:', '', ids)
	}

	# Search by mass
	if ( ! is.null(mass.field))
		.self$message('caution', paste0('Mass search is not handled.'))

	# Cut
	if ( ! is.na(max.results) && max.results > 0 && max.results < length(ids))
		ids <- ids[1:max.results]

	return(ids)
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
