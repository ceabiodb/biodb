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
#' @seealso \code{\link{BiodbFactory}}, \code{\link{KeggConn}}, \code{\link{BiodbCompoundbConn}}.
#'
#' @include KeggConn.R
#' @include BiodbCompounddbConn.R
#' @export KeggEnzymeConn
#' @exportClass KeggEnzymeConn
KeggEnzymeConn <- methods::setRefClass("KeggEnzymeConn", contains = c("KeggConn", "BiodbCompounddbConn"))

# Constructor {{{1
################################################################

KeggEnzymeConn$methods( initialize = function(...) {
	callSuper(db.name = 'enzyme', db.abbrev = 'ec', ...)
})

# Search compound {{{1
################################################################

KeggEnzymeConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.field = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {
		
	.self$.checkMassField(mass = mass, mass.field = mass.field)

	ids <- NULL

	# Search by name
	if ( ! is.null(name) && ! is.na(name)) {
		ids <- .self$ws.find(name, retfmt = 'ids')
		ids <- sub('^ec:', '', ids)
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
