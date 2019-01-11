# vi: fdm=marker

# Constants {{{1
################################################################

.BIODB.PEAKFOREST.COMPOUND.PARSING.EXPR <- list(
	'accession'             = "id",
	'ncbi.pubchem.comp.id'  = "PubChemCID",
	'chebi.id'              = "ChEBI",
	'hmdb.metabolites.id'   = "HMDB",
	'kegg.compound.id'      = "KEGG",
	'formula'               = "formula",
	'smiles'                = "canSmiles",
	'average.mass'          = "averageMass",
	'monoisotopic.mass'     = "monoisotopicMass",
	'inchi'                 = "inChI",
	'inchikey'              = "inChIKey",
	'name'                  = "mainName",
	'logp'                  = "logP"
)

# Class declaration {{{1
################################################################

#' @include PeakforestConn.R
#' @include CompounddbConn.R
PeakforestCompoundConn <- methods::setRefClass("PeakforestCompoundConn", contains = c("PeakforestConn", "CompounddbConn"))

# Constructor {{{1
################################################################

PeakforestCompoundConn$methods( initialize = function(...) {

	callSuper(db.name = 'compounds', ...)
})

# Get entry content url {{{1
################################################################

PeakforestCompoundConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {

	# Check token
	if (is.na(.self$getToken()))
		.self$message('error', "Peakforest requires a token for this service.")

	url <- paste(.self$getBaseUrl(), 'compounds/', id,'?token=', .self$getToken(), sep = '')

	return(url)
})

# Get entry page url {{{1
################################################################

PeakforestCompoundConn$methods( getEntryPageUrl = function(id) {
	return(paste('https://metabohub.peakforest.org/webapp/home?PFc=', id))
})

# Get entry image url {{{1
################################################################

PeakforestCompoundConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})

# Web service search.compounds.mass {{{1
################################################################

PeakforestCompoundConn$methods( ws.search.compounds.mass = function(field, mass, delta, max = NA_integer_, biodb.parse = FALSE, biodb.ids = FALSE) {

	# Check mass field
	if ( ! field %in% c('monoisotopicmass', 'averagemass'))
		.self$message('error', paste0('Unknown mass field "', field, '".'))

	# Build request
	url <- paste0(.self$getBaseUrl(), 'search/compounds/', field, '/', mass, '/', delta)
	params <- c(token = .self$getToken())
	if ( ! is.na(max))
		params <- c(params, max = max)

	# Send request
	results <- .self$.getUrlScheduler()$getUrl(url, params = params)

	# Parse results
	if (biodb.parse || biodb.ids)
		results <- jsonlite::fromJSON(results, simplifyDataFrame = FALSE)

	# Extract IDs
	if (biodb.ids) {
		if ('compounds' %in% names(results))
			results <- vapply(results$compounds, function(x) as.character(x$id), FUN.VALUE = '')
		else
			.self$message('error', 'Could find "compounds" field inside returned JSON.')
	}

	return(results)
})

# Search compound {{{1
################################################################

PeakforestCompoundConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.field = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {
		
	.self$.checkMassField(mass = mass, mass.field = mass.field)

	ids <- NULL

	# Will we search for mass
	search.mass <- FALSE
	if ( ! is.null(mass.field) && ! is.null(mass)) {
		if (mass.field %in% c('monoisotopic.mass', 'average.mass'))
			search.mass <- TRUE
		else
			.self$message('caution', paste0('Mass field ', mass.field, ' is not handled.'))
	}

	# Search by name
	if ( ! is.null(name)) {
		max <- if (search.mass) NA_integer_ else max.results
		ids <- .self$ws.search(name, max = max, biodb.ids = TRUE)
	}

	# Search by mass
	if (search.mass) {
		if (mass.tol.unit == 'ppm')
			delta <- mass * mass.tol * 1e-6
		else
			delta <- mass.tol
		field <- if (mass.field == 'monoisotopic.mass') 'monoisotopicmass' else 'averagemass'
		max <- if (is.null(name)) max.results else NA_integer_
		mass.ids <- .self$ws.search.compounds.mass(field = field, mass = mass, delta = delta, max = max, biodb.ids = TRUE)
		if ( ! is.null(ids))
			ids <- ids[ids %in% mass.ids]
		else
			ids <- mass.ids
	}

	# Cut
	if ( ! is.na(max.results) && length(ids) > max.results)
		ids <- ids[1:max.results]

	return(ids)
})

# Private methods {{{1
################################################################

# Get parsing expressions {{{2
################################################################

PeakforestCompoundConn$methods( .getParsingExpressions = function() {
	return(.BIODB.PEAKFOREST.COMPOUND.PARSING.EXPR)
})
