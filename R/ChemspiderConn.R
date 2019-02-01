# vi: fdm=marker

# Constants {{{1
################################################################

.BIODB.CHEMSPIDER.PARSING.EXPR <- list(
	'accession'         = "id",
	'formula'           = "formula",
	'name'              = "commonName",
	'average.mass'      = "averageMass",
	'monoisotopic.mass' = "monoisotopicMass",
	'nominal.mass'      = "nominalMass",
	'molecular.weight'  = "molecularWeight",
	'inchi'             = "inchi",
	'inchikey'          = "inchiKey",
	'smiles'            = "smiles"
)

# Class declaration {{{1
################################################################

#' The connector class to ChemSpider database.
#'
#' This is a concrete connector class. It must never be instantiated directly, but instead be instantiated through the factory \code{\link{BiodbFactory}}. Only specific methods are described here. See super classes for the description of inherited methods.
#'
#' @param mass   The mass to search for.
#' @param query  The query to send to the database.
#' @param range  The range of the searched mass. Plain range, Dalton unit. The mass searched are between (mass - range) and (mass + range).
#' @param retfmt The wanted returned format, in a web service method ("ws.*" methods).
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{RemotedbConn}}, \code{\link{CompounddbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include CompounddbConn.R
#' @include RemotedbConn.R
#' @export ChemspiderConn
#' @exportClass ChemspiderConn
ChemspiderConn <- methods::setRefClass("ChemspiderConn", contains = c("RemotedbConn", "CompounddbConn"))

# Get entry content {{{1
################################################################

ChemspiderConn$methods( getEntryContent = function(entry.id) {

	# Initialize return values
	content <- rep(NA_character_, length(entry.id))

	# Get requests
	requests <- .self$getEntryContentRequest(entry.id, concatenate = TRUE)

	# Loop on all requests
	for (request in requests) {

		# Send request
		results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

		if ( ! is.null(results) && ! is.na(results)) {

			# Parse results
			results <- jsonlite::fromJSON(results, simplifyDataFrame = FALSE)

			# Multiple records
			records <- if ('records' %in% names(results)) results[['records']] else list(results)

			# Store record contents
			for (record in records)
				if ('id' %in% names(record))
					content[entry.id == record$id] <- jsonlite::toJSON(record, pretty = TRUE, digits = NA_integer_)
		}
	}

	return(content)
})


# Do get entry content request {{{1
################################################################

ChemspiderConn$methods( .doGetEntryContentRequest = function(id, concatenate = TRUE) {

	# Use batch requests
	if (concatenate) {

		# Divide IDs into group of max 100
		id.chunks <- split(id, ceiling(seq_along(id) / 100))

		# Create requests
		requests <- lapply(id.chunks, function(x) .self$ws.recordsBatchPost(x, retfmt = 'request'))
	}

	# One request for each ID
	else
		requests <- lapply(id, function(x) .self$ws.recordsRecordidDetailsGet(x, retfmt = 'request'))

	return(requests)
})

# Get entry page url {{{1
################################################################

ChemspiderConn$methods( getEntryPageUrl = function(id) {
	return(paste0(.self$getBaseUrl(), 'Chemical-Structure.', id, '.html'))
})

# Get entry image url {{{1
################################################################

ChemspiderConn$methods( getEntryImageUrl = function(id) {
	return(paste(.self$getBaseUrl(), 'ImagesHandler.ashx?w=300&h=300&id=', id, sep = ''))
})

# Get all record fields {{{1
################################################################

ChemspiderConn$methods( getAllRecordFields = function() {
	":\n\nReturns the complete list of all record fields provided by ChemSpider."

	return(c('SMILES', 'Formula', 'InChI', 'InChIKey', 'StdInChI', 'StdInChIKey', 'AverageMass', 'MolecularWeight', 'MonoisotopicMass', 'NominalMass', 'CommonName', 'ReferenceCount', 'DataSourceCount', 'PubMedCount', 'RSCCount', 'Mol2D', 'Mol3D'))
})

# Web service records-recordId-details-get {{{1
################################################################

ChemspiderConn$methods( ws.recordsRecordidDetailsGet = function(recordid, fields = NULL, retfmt = c('plain', 'parsed', 'request')) {
	":\n\nAccess the records-recordId-details-get ChemSpider web service. See https://developer.rsc.org/compounds-v1/apis/get/records/{recordId}/details."

	retfmt <- match.arg(retfmt)

	# Convert ID to integer
	recordid <- suppressWarnings(as.integer(recordid))

	# Get fields to retrieve
	if (is.null(fields))
		fields <- paste(.self$getAllRecordFields(), collapse = ',')

	# Build request
	header <- c('Content-Type' = "", apikey = .self$getToken())
	request <- BiodbRequest(method = 'get', url = BiodbUrl(paste0(.self$getUrl('ws.url'), 'records/', recordid, '/details'), params = c(fields = fields)), header = header)
	if (retfmt == 'request')
		return(request)

	# Send request
	results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

	# Error
	if (is.null(results) || is.na(results))
		results <- NULL
	else if (retfmt == 'parsed')
		results <- jsonlite::fromJSON(results, simplifyDataFrame = FALSE)

	return(results)
})

# Web service records-batch-post {{{1
################################################################

ChemspiderConn$methods( ws.recordsBatchPost = function(recordids, fields = NULL, retfmt = c('plain', 'parsed', 'request')) {
	":\n\nAccess the filter-name-post ChemSpider web service. See https://developer.rsc.org/compounds-v1/apis/post/records/batch."

	retfmt <- match.arg(retfmt)

	# Convert IDs to integer
	recordids <- suppressWarnings(as.integer(recordids))
	recordids <- recordids[ ! is.na(recordids)]

	# Get fields to retrieve
	if (is.null(fields))
		fields <- .self$getAllRecordFields()

	# Build request
	header <- c('Content-Type' = "", apikey = .self$getToken())
	body <- paste0('{"recordIds": [', paste(recordids, collapse = ','), '], "fields": [', paste(vapply(fields, function(x) paste0('"', x, '"'), FUN.VALUE = ''), collapse = ',') ,']}')
	request <- BiodbRequest(method = 'post', url = BiodbUrl(paste0(.self$getUrl('ws.url'), 'records/batch')), header = header, body = body)
	if (retfmt == 'request')
		return(request)

	# Send request
	results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

	# Error
	if (is.null(results) || is.na(results))
		results <- NULL
	else if (retfmt == 'parsed')
		results <- jsonlite::fromJSON(results, simplifyDataFrame = FALSE)

	return(results)
})

# Web service filter-name-post {{{1
################################################################

ChemspiderConn$methods( ws.filterNamePost = function(name, retfmt = c('plain', 'parsed', 'queryid', 'ids', 'request')) {
	":\n\nAccess the filter-name-post ChemSpider web service. See https://developer.rsc.org/compounds-v1/apis/post/filter/name."

	retfmt <- match.arg(retfmt)

	# Build request
	header <- c('Content-Type' = "", apikey = .self$getToken())
	body <- paste0("{\n", '\t"name": "', name, '"', "\n}")
	request <- BiodbRequest(method = 'post', url = BiodbUrl(paste0(.self$getUrl('ws.url'), 'filter/name')), header = header, body = body)
	if (retfmt == 'request')
		return(request)

	# Send request
	results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

	# Error
	if (is.null(results) || is.na(results))
		results <- NULL
	else
		results <- .self$.retrieveQuery(results = results, retfmt = retfmt)

	return(results)
})

# Web service filter-mass-post {{{1
################################################################

ChemspiderConn$methods( ws.filterMassPost = function(mass, range, retfmt = c('plain', 'parsed', 'queryid', 'ids', 'request')) {
	":\n\nAccess the filter-mass-post ChemSpider web service. See https://developer.rsc.org/compounds-v1/apis/post/filter/mass."

	retfmt <- match.arg(retfmt)

	# Build request
	header <- c('Content-Type' = "", apikey = .self$getToken())
	body <- paste0("{\n", '\t"mass": ', mass, ",\n",'\t"range": ', range, "\n}")
	request <- BiodbRequest(method = 'post', url = BiodbUrl(paste0(.self$getUrl('ws.url'), 'filter/mass')), header = header, body = body)
	if (retfmt == 'request')
		return(request)

	# Send request
	results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

	# Error
	if (is.null(results) || is.na(results))
		results <- NULL
	else
		results <- .self$.retrieveQuery(results = results, retfmt = retfmt)

	return(results)
})

# Web service filter-queryId-status-get {{{1
################################################################

ChemspiderConn$methods( ws.filterQueryIdStatusGet = function(queryid, retfmt = c('plain', 'parsed', 'status', 'request')) {
	":\n\nAccess the filter-queryId-status-get ChemSpider web service. See https://developer.rsc.org/compounds-v1/apis/get/filter/{queryId}/status."

	.self$.assert.not.null(queryid)
	.self$.assert.not.na(queryid)
	.self$.assert.is(queryid, 'character')

	retfmt <- match.arg(retfmt)

	# Set URL
	header <- c('Content-Type' = "", apikey = .self$getToken())
	request <- BiodbRequest(method = 'get', url = BiodbUrl(paste0(.self$getUrl('ws.url'), 'filter/', queryid, '/status')), header = header)
	if (retfmt == 'request')
		return(request)

	# Send request
	results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request, cache = FALSE)

	# Parse JSON
	if (retfmt != 'plain') {

		results <- jsonlite::fromJSON(results, simplifyDataFrame = FALSE)

		# Get status
		if (retfmt == 'status')
			results <- results$status
	}

	return(results)
})

# Web service filter-queryId-results-get {{{1
################################################################

ChemspiderConn$methods( ws.filterQueryIdResultsGet = function(queryid, start = 0L, count = 0L, retfmt = c('plain', 'parsed', 'ids', 'request')) {
	":\n\nAccess the filter-queryId-results-get ChemSpider web service. See https://developer.rsc.org/compounds-v1/apis/get/filter/{queryId}/results,"

	.self$.assert.not.null(queryid)
	.self$.assert.not.na(queryid)
	.self$.assert.is(queryid, 'character')
	.self$.assert.number(start, negative = FALSE, float.allowed = FALSE)
	.self$.assert.number(count, negative = FALSE, float.allowed = FALSE)

	retfmt <- match.arg(retfmt)

	# Build request
	url <- BiodbUrl(paste0(.self$getUrl('ws.url'), 'filter/', queryid, '/results'))
	if (start > 0)
		url$setParam('start', start)
	if (count > 0)
		url$setParam('count', count)
	header <- c('Content-Type' = "", apikey = .self$getToken())
	request <- BiodbRequest(method = 'get', url = url, header = header)
	if (retfmt == 'request')
		return(request)

	# Send request
	results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

	# Parse JSON
	if (retfmt != 'plain') {

		results <- jsonlite::fromJSON(results, simplifyDataFrame = FALSE)

		# Parse IDs
		if (retfmt == 'ids')
			results <- results$results
	}

	return(results)
})

# Search compound {{{1
################################################################

ChemspiderConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.field = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {
		
	.self$.checkMassField(mass = mass, mass.field = mass.field)

	ids <- NULL

	# Send request on mass
	if ( ! is.null(mass) && ! is.null(mass.field)) {
		mass.field <- .self$getBiodb()$getEntryFields()$getRealName(mass.field)
		if (mass.field == 'monoisotopic.mass') {
			if (mass.tol.unit == 'ppm')
				range <- mass * mass.tol * 1.e-6
			else
				range <- mass.tol
			ids <- .self$ws.filterMassPost(mass = mass, range = range, retfmt = 'ids')
		}
		else
			.self$message('caution', paste0('Mass field "', mass.field, '" is not handled.'))
	}

	# Search by name
	if ( ! is.null(name)) {

		name.id <- .self$ws.filterNamePost(name, retfmt = 'ids')

		# Merge with already found IDs
		if (is.null(ids))
			ids <- name.id
		else
			ids <- ids[ids %in% name.id]
	}

	# Cut
	if ( ! is.null(ids) && ! is.na(max.results) && max.results > 0 && max.results < length(ids))
		ids <- ids[1:max.results]

	return(ids)
})

# Get entry ids {{{1
################################################################

ChemspiderConn$methods( getEntryIds = function(max.results = NA_integer_) {
	"This method is not correctly implemented. This is because ChemSpider API does not provide a service for obtaining the exact number of entries. As a consequence we use `searchEntryByMass()` method to search for entries. However, since looking for all entries this way makes ChemSpider fails with `System.OutOfMemoryException`, only a subset of the entries is retrieve. This method, implemented this way, is still useful for testing purposes."

	.self$message('caution', "Method using a last resort solution for its implementation. Returns only a small subset of ChemSpider entries.")

	ids <- NULL

	if ( ! is.na(max.results))
		mass.tol <- if (max.results <= 100) 0.01 else (0.01 * max.results / 100)
	else
		mass.tol <- 10
	ids <- .self$searchCompound(mass = 100, mass.field = 'monoisotopic.mass', mass.tol = mass.tol, max.results = max.results)

	return(ids)
})

# Private methods {{{1
################################################################

# Get parsing expressions {{{2
################################################################

ChemspiderConn$methods( .getParsingExpressions = function() {
	return(.BIODB.CHEMSPIDER.PARSING.EXPR)
})

# Retrieve query {{{2
################################################################

ChemspiderConn$methods( .retrieveQuery = function(results, retfmt) {

	# Parse JSON
	if (retfmt != 'plain') {

		results <- jsonlite::fromJSON(results, simplifyDataFrame = FALSE)

		if (retfmt == 'queryid')
			results <- results$queryId

		# Get results
		else if (retfmt == 'ids') {

			# Wait for query result to be ready
			while (TRUE) {
				status <- .self$ws.filterQueryIdStatusGet(results$queryId, retfmt = 'status')
				if (is.null(status))
					return(NULL)

				if (status == 'Complete')
					break
			}

			# Get results
			ids <- NULL
			while (TRUE) {

				res <- .self$ws.filterQueryIdResultsGet(results$queryId, retfmt = 'parsed')
				ids <- c(ids, res$results)

				if ( ! res$limitedToMaxAllowed)
					break
			}
			results <- as.character(ids)
		}
	}

	return(results)
})
