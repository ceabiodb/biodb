# vi: fdm=marker

# Constants {{{1
################################################################

.BIODB.KEGG.COMPOUND.PARSING.EXPR <- list(
	'accession'         = "^ENTRY\\s+(\\S+)\\s+Compound",
	'name'              = "^NAME\\s+([^,;]+)",
	'formula'           = "^FORMULA\\s+(\\S+)$",
	'exact.mass'        = "^EXACT_MASS\\s+(\\S+)$",
	'molecular.weight'  = "^MOL_WEIGHT\\s+(\\S+)$"
)

# Class declaration {{{1
################################################################

#' The connector class to KEGG Compound database.
#'
#' This is a concrete connector class. It must never be instantiated directly, but instead be instantiated through the factory \code{\link{BiodbFactory}}. Only specific methods are described here. See super classes for the description of inherited methods.
#'
#' @param mass      Single mass.
#' @param mass.min  Minimal mass.
#' @param mass.max  Maximal mass.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{KeggConn}}, \code{\link{CompounddbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector to KEGG Compound
#' conn <- mybiodb$getFactory()$createConn('kegg.compound')
#'
#' # Search for compounds by exact mass
#' conn$ws.find.exact.mass.df(mass = 174.05)
#'
#' # Search for compounds by molecular weight 
#' conn$ws.find.molecular.weight.df(mass = 300)
#'
#' # Terminate instance.
#' mybiodb$terminate()
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
	":\n\nSearch for entries by mass. See http://www.kegg.jp/kegg/docs/keggapi.html for details."

	if ( ! is.na(mass))
		url <- paste(.self$getWsUrl(), 'find/', .self$.db.name, '/', mass, '/exact_mass', sep ='')
	else if ( ! is.na(mass.min) && ! is.na(mass.max))
		url <- paste(.self$getWsUrl(), 'find/', .self$.db.name, '/', mass.min, '-', mass.max, '/exact_mass', sep = '')
	else
		.self$message('error', 'You need to specify either mass parameter or both mass.min and mass.max.')

	result <- .self$.getUrlScheduler()$getUrl(url)

	return(result)
})

# Web service find exact mass DF {{{1
################################################################

KeggCompoundConn$methods( ws.find.exact.mass.df = function(...) {
	":\n\nCalls ws.find.exact.mass() and returns a data frame."

	results <- .self$ws.find.exact.mass(...)

	readtc <- textConnection(results, "r", local = TRUE)
	df <- read.table(readtc, sep = "\t", quote = '', stringsAsFactors = FALSE)

	return(df)
})

# Web service find exact mass IDs {{{1
################################################################

KeggCompoundConn$methods( ws.find.exact.mass.ids = function(...) {
	":\n\nCalls ws.find.exact.mass() but only for getting IDs. Returns the IDs as a character vector."

	df <- .self$ws.find.exact.mass.df(...)

	return(df[[1]])
})

# Web service find molecural weight {{{1
################################################################

KeggCompoundConn$methods( ws.find.molecular.weight = function(mass = NA_real_, mass.min = NA_real_, mass.max = NA_real_) {
	":\n\nSearch for entries by molecular mass. See http://www.kegg.jp/kegg/docs/keggapi.html for details."

	if ( ! is.na(mass))
		url <- paste(.self$getWsUrl(), 'find/', .self$.db.name, '/', mass, '/mol_weight', sep ='')
	else if ( ! is.na(mass.min) && ! is.na(mass.max))
		url <- paste(.self$getWsUrl(), 'find/', .self$.db.name, '/', mass.min, '-', mass.max, '/mol_weight', sep = '')
	else
		.self$message('error', 'You need to specify either mass parameter or both mass.min and mass.max.')

	result <- .self$.getUrlScheduler()$getUrl(url)

	return(result)
})

# Web service find molecular weight DF {{{1
################################################################

KeggCompoundConn$methods( ws.find.molecular.weight.df = function(...) {
	":\n\nCalls ws.find.molecular.weight() and returns a data frame."

	results <- .self$ws.find.molecular.weight(...)

	readtc <- textConnection(results, "r", local = TRUE)
	df <- read.table(readtc, sep = "\t", quote = '', stringsAsFactors = FALSE)

	return(df)
})

# Web service find molecular weight IDs {{{1
################################################################

KeggCompoundConn$methods( ws.find.molecular.weight.ids = function(...) {
	":\n\nCalls ws.find.molecular.weight() but only for getting IDs. Returns the IDs as a character vector."

	df <- .self$ws.find.molecular.weight.df(...)

	return(df[[1]])
})

# Search compound {{{1
################################################################

KeggCompoundConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.field = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {
		
	.self$.checkMassField(mass = mass, mass.field = mass.field)

	ids <- NULL

	# Search by name
	if ( ! is.null(name) && ! is.na(name)) {
		ids <- .self$ws.find.ids(name)
		ids <- sub('^cpd:', '', ids)
	}

	# Search by mass
	if ( ! is.null(mass)) {

		mass.field <- .self$getBiodb()$getEntryFields()$getRealName(mass.field)

		if ( ! mass.field %in% c('monoisotopic.mass' ,'molecular.mass'))
			.self$message('caution', paste0('Mass field "', mass.field, '" is not handled.'))

		else {

			if (mass.tol.unit == 'ppm') {
				mass.min <- mass * (1 - mass.tol * 1e-6)
				mass.max <- mass * (1 + mass.tol * 1e-6)
			} else {
				mass.min <- mass - mass.tol
				mass.max <- mass + mass.tol
			}

			if (mass.field == 'monoisotopic.mass')
				mass.ids <- .self$ws.find.exact.mass.ids(mass.min = mass.min, mass.max = mass.max)
			else
				mass.ids <- .self$ws.find.molecular.weight.ids(mass.min = mass.min, mass.max = mass.max)
			.self$message('debug', paste('Got entry IDs ', paste(mass.ids, collapse = ', '), '.')) 
			if ( ! is.null(mass.ids) && any(! is.na(mass.ids))) {
				mass.ids <- sub('^cpd:', '', mass.ids)
				if (is.null(ids))
					ids <- mass.ids
				else
					ids <- ids[ids %in% mass.ids]
			}
		}
	}

	# Cut
	if ( ! is.na(max.results) && max.results > 0 && max.results < length(ids))
		ids <- ids[1:max.results]

	return(ids)
})

# Get entry image url {{{1
################################################################

KeggCompoundConn$methods( getEntryImageUrl = function(id) {
	return(paste(.self$getBaseUrl(), 'Fig/compound/', id, '.gif', sep = ''))
})

# Private methods {{{1
################################################################

# Get parsing expressions {{{2
################################################################

KeggCompoundConn$methods( .getParsingExpressions = function() {
	return(.BIODB.KEGG.COMPOUND.PARSING.EXPR)
})
