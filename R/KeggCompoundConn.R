# vi: fdm=marker

# Constants {{{1
################################################################

.BIODB.KEGG.COMPOUND.PARSING.EXPR <- list(
	'accession'              = "^ENTRY\\s+(\\S+)\\s+Compound",
	'formula'                = "^FORMULA\\s+(\\S+)$",
	'exact.mass'             = "^EXACT_MASS\\s+(\\S+)$",
	'molecular.weight'       = "^MOL_WEIGHT\\s+(\\S+)$",
	'cas.id'                 = "^[DBLINKS ]+ CAS:\\s+(\\S+)$",
	'ncbi.pubchem.comp.id'   = "^[DBLINKS ]+ PubChem:\\s+(\\S+)$",
	'chebi.id'               = "^[DBLINKS ]+ ChEBI:\\s+(\\S+)$",
	'lipidmaps.structure.id' = "^[DBLINKS ]+ LIPIDMAPS:\\s+(\\S+)$"
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
#' @param id        A character vector of entry IDs.
#' @param org       The organism in which to search for pathways, as a KEGG
#'                  organism code (3-4 letters code, like "hsa", "mmu", ...).
#'                  See https://www.genome.jp/kegg/catalog/org_list.html for a
#'                  complete list of KEGG organism codes.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{KeggConn}}, \code{\link{BiodbCompounddbConn}}, \code{\link{KeggPathwayConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector to KEGG Compound
#' conn <- mybiodb$getFactory()$createConn('kegg.compound')
#'
#' # Search for compounds by exact mass
#' conn$ws.find.exact.mass(mass = 174.05, retfmt = 'parsed')
#'
#' # Search for compounds by molecular weight 
#' conn$ws.find.molecular.weight(mass = 300, retfmt = 'parsed')
#'
#' # Get pathway IDs related to compounds
#' pathway.ids = conn$getPathwayIds(c('C02648', 'C06144'), org = 'mmu')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include KeggConn.R
#' @include BiodbCompounddbConn.R
#' @export KeggCompoundConn
#' @exportClass KeggCompoundConn
KeggCompoundConn <- methods::setRefClass("KeggCompoundConn", contains = c("KeggConn", "BiodbCompounddbConn"))

# Constructor {{{1
################################################################

KeggCompoundConn$methods( initialize = function(...) {
	callSuper(db.name = 'compound', db.abbrev = 'cpd', ...)
})

# Web service find exact mass {{{1
################################################################

KeggCompoundConn$methods( ws.find.exact.mass = function(mass = NA_real_, mass.min = NA_real_, mass.max = NA_real_, retfmt = c('plain', 'request', 'parsed', 'ids')) {
	":\n\nSearch for entries by mass. See http://www.kegg.jp/kegg/docs/keggapi.html for details."

	retfmt = match.arg(retfmt)

	# Build request
	if ( ! is.na(mass))
		url = BiodbUrl(url = c(.self$getUrl('ws.url'), 'find', .self$.db.name, mass, 'exact_mass'))$toString()
	else if ( ! is.na(mass.min) && ! is.na(mass.max))
		url = BiodbUrl(url = c(.self$getUrl('ws.url'), 'find', .self$.db.name, paste(mass.min, mass.max, sep = '-'), 'exact_mass'))$toString()
	else
		.self$message('error', 'You need to specify either mass parameter or both mass.min and mass.max.')
	request = BiodbRequest(method = 'get', url = BiodbUrl(url = url))
	if (retfmt == 'request')
		return(request)

	# Send request
	results = .self$getBiodb()$getRequestScheduler()$sendRequest(request)

	# Parse results
	if (retfmt != 'plain') {

		# Parse
		if (length(grep('^[[:space:]]*$', results, perl = TRUE)) == 0) {
			readtc = textConnection(results, "r", local = TRUE)
			df = read.table(readtc, sep = "\t", quote = '', stringsAsFactors = FALSE)
			close(readtc)
			results = df
		} else {
			results = data.frame()
		}

		# Get IDs
		if (retfmt == 'ids')
			results = if (ncol(results) > 0) results[[1]] else character()
	}

	return(results)
})

# Web service find molecural weight {{{1
################################################################

KeggCompoundConn$methods( ws.find.molecular.weight = function(mass = NA_real_, mass.min = NA_real_, mass.max = NA_real_, retfmt = c('plain', 'request', 'parsed', 'ids')) {
	":\n\nSearch for entries by molecular mass. See http://www.kegg.jp/kegg/docs/keggapi.html for details."

	retfmt = match.arg(retfmt)

	# Build request
	if ( ! is.na(mass))
		url = BiodbUrl(url = c(.self$getUrl('ws.url'), 'find', .self$.db.name, mass, 'mol_weight'))$toString()
	else if ( ! is.na(mass.min) && ! is.na(mass.max))
		url = BiodbUrl(url = c(.self$getUrl('ws.url'), 'find', .self$.db.name, paste(mass.min, mass.max, sep = '-'), 'mol_weight'))$toString()
	else
		.self$message('error', 'You need to specify either mass parameter or both mass.min and mass.max.')
	request = BiodbRequest(method = 'get', url = BiodbUrl(url = url))
	if (retfmt == 'request')
		return(request)

	# Send request
	results = .self$getBiodb()$getRequestScheduler()$sendRequest(request)

	# Parse results
	if (retfmt != 'plain') {

		# Parse
		if (length(grep('^[[:space:]]*$', results, perl = TRUE)) == 0) {
			readtc = textConnection(results, "r", local = TRUE)
			df = read.table(readtc, sep = "\t", quote = '', stringsAsFactors = FALSE)
			close(readtc)
			results = df
		} else {
			results = data.frame()
		}

		# Get IDs
		if (retfmt == 'ids')
			results = if (ncol(results) > 0) results[[1]] else character()
	}

	return(results)
})

# Search compound {{{1
################################################################

KeggCompoundConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.field = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {
		
	.self$.checkMassField(mass = mass, mass.field = mass.field)

	ids <- NULL

	# Search by name
	if ( ! is.null(name) && ! is.na(name))
		ids = .self$searchByName(name)

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
				mass.ids = .self$ws.find.exact.mass(mass.min = mass.min, mass.max = mass.max, retfmt = 'ids')
			else
				mass.ids = .self$ws.find.molecular.weight(mass.min = mass.min, mass.max = mass.max, retfmt = 'ids')
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
	return(vapply(id, function(x) BiodbUrl(url = c(.self$getUrl('base.url'), 'Fig', 'compound', paste(x, 'gif', sep = '.')))$toString(), FUN.VALUE = ''))
})

# Get pathway IDs per compound {{{1
################################################################

KeggCompoundConn$methods( getPathwayIdsPerCompound = function(id, org) {
	"Get organism pathways for each compound. Given a vector of KEGG Compound IDs and a KEGG organism code, this method retrieves for each compound the KEGG pathways of the organism in which the compound is involved. It returns a named list of KEGG pathway ID vectors, where the names of the list are the compound IDs."

	pathways = list()

    kegg.enz.conn = .self$getBiodb()$getFactory()$getConn('kegg.enzyme')
    kegg.path.conn = .self$getBiodb()$getFactory()$getConn('kegg.pathway')
    kegg.module.conn = .self$getBiodb()$getFactory()$getConn('kegg.module')
    
	# Loop on all compound ids
    i <- 0
	for (comp.id in id) {

		# Get compound entry
		comp = .self$getEntry(comp.id)
		if ( ! is.null(comp) && comp$hasField('kegg.enzyme.id')) {

			# Get pathways
			pws = kegg.enz.conn$getPathwayIds(comp$getFieldValue('kegg.enzyme.id'), org = org)

			# Loop on all pathways
			good_pws = character()
			for (pw in pws) {
				
				# Get pathway entry
				pw.entry <- kegg.path.conn$getEntry(pw)
				if ( ! is.null(pw.entry)) {
					# Check that the compound is inside the list of compounds
					if (pw.entry$hasField('kegg.compound.id') && comp.id %in% pw.entry$getFieldValue('kegg.compound.id'))
						good_pws = c(good_pws, pw)
					else if (pw.entry$hasField('kegg.module.id')) {
						# We need to check that the compound is listed in at least one of the modules
						module.ids <- pw.entry$getFieldValue('kegg.module.id')
						for (m in module.ids) {
							mentry <- kegg.module.conn$getEntry(m)
							if ( ! is.null(mentry) && mentry$hasField('kegg.compound.id') && comp.id %in% mentry$getFieldValue('kegg.compound.id'))
								good_pws = c(good_pws, pw)
						}
					}
				}
			}

			# Record good pathways
			pathways[[comp.id]] = good_pws
		}

		# Send progress message
		i <- i + 1
		lapply(.self$getBiodb()$getObservers(), function(x) x$progress(type = 'info', msg = 'Retrieving pathways of compounds.', index = i, total = length(id), first = (i == 1)))
	}

	return(pathways)
})

# Get pathway IDs {{{1
################################################################

KeggCompoundConn$methods( getPathwayIds = function(id, org) {
	"Get organism pathways. Given a vector of KEGG Compound IDs and a KEGG organism code, this method retrieves KEGG pathways of this organism in which the compounds are involved. It returns a vector of KEGG pathway IDs."

	pathways = .self$getPathwayIdsPerCompound(id = id, org = org)
	pathways = unique(unlist(pathways, use.names = FALSE))

	return(pathways)
})

# Private methods {{{1
################################################################

# Get parsing expressions {{{2
################################################################

KeggCompoundConn$methods( .getParsingExpressions = function() {
	return(.BIODB.KEGG.COMPOUND.PARSING.EXPR)
})
