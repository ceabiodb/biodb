# vi: fdm=marker

# Class declaration {{{1
################################################################

#' The connector class to KEGG Compound database.
#'
#' This is a concrete connector class. It must never be instantiated directly, but instead be instantiated through the factory \code{\link{BiodbFactory}}. Only specific methods are described here. See super classes for the description of inherited methods.
#'
#' @param
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{RemotedbConn}}, \code{\link{CompounddbConn}}.
#'
#' @examples
#' # TODO
#'
#' @include KeggConn.R
#' @include CompounddbConn.R
#' @export KeggCompoundConn
#' @exportClass KeggCompoundConn
KeggCompoundConn <- methods::setRefClass("KeggCompoundConn", contains = c("KeggConn", "CompounddbConn"))

# Constructor {{{1
################################################################

KeggCompoundConn$methods( initialize = function(...) {
	callSuper(db.name = 'compound', db.abbrev = 'cpd', ...)
})

# Web service find exact mass {{{1
################################################################

KeggCompoundConn$methods( ws.find.exact.mass = function(mass = NA_real_, mass.min = NA_real_, mass.max = NA_real_) {
	":\n\nSearch for entries by mass."

	if ( ! is.na(mass))
		url <- paste(.self$getBaseUrl(), 'find/', .self$.db.name, '/', mass, '/exact_mass', sep ='')
	else if ( ! is.na(mass.min) && ! is.na(mass.max))
		url <- paste(.self$getBaseUrl(), 'find/', .self$.db.name, '/', mass.min, '-', mass.max, '/exact_mass', sep = '')
	else
		.self$message('error', 'You need to specify either mass parameter or both mass.min and mass.max.')

	result <- .self$.getUrlScheduler()$getUrl(url)

	return(result)
})

# Search compound {{{1
################################################################

KeggCompoundConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {
})
