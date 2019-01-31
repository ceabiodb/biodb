# vi: fdm=marker

# Constants {{{1
################################################################

.BIODB.CHEMSPIDER.PARSING.EXPR <- list(
	'accession'         = "//CSID",
	'formula'           = "//MF",
	'name'              = "//CommonName",
	'average.mass'      = "//AverageMass",
	'monoisotopic.mass' = "//MonoisotopicMass",
	'nominal.mass'      = "//NominalMass",
	'molecular.weight'  = "//MolecularWeight",
	'inchi'             = "//InChI",
	'inchikey'          = "//InChIKey",
	'smiles'            = "//SMILES"
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

	# Debug
	.self$message('info', paste0("Get entry content(s) for ", length(entry.id)," id(s)..."))

	URL.MAX.LENGTH <- 2083
	concatenate <- TRUE
	done <- FALSE

	while ( ! done) {

		done <- TRUE

		# Initialize return values
		content <- rep(NA_character_, length(entry.id))

		# Get request URLs
		urls <- .self$getEntryContentUrl(entry.id, concatenate = concatenate, max.length = URL.MAX.LENGTH)

		# Loop on all URLs
		for (url in urls) {

			# Send request
			xmlstr <- .self$.getUrlScheduler()$getUrl(url)

			# Error : "Cannot convert WRONG to System.Int32.\r\nParameter name: type ---> Input string was not in a correct format.\r\n"
			if (grepl('^Cannot convert .* to System\\.Int32\\.', xmlstr)) {
				if (concatenate) {
					.self$message('caution', "One of the IDs to retrieve is wrong.")
					concatenate <- FALSE
					done <- FALSE
					break
				}
				next
			}

			# Parse XML and get included XML
			if ( ! is.na(xmlstr)) {
				xml <-  XML::xmlInternalTreeParse(xmlstr, asText = TRUE)
				returned.ids <- XML::xpathSApply(xml, "//ns:ExtendedCompoundInfo/ns:CSID", XML::xmlValue, namespaces = c(ns = .self$getXmlNs()))
				content[match(returned.ids, entry.id)] <- vapply(XML::getNodeSet(xml, "//ns:ExtendedCompoundInfo", namespaces = c(ns = .self$getXmlNs())), XML::saveXML, FUN.VALUE = '')
			}
		}
	}

	return(content)
})


# Do get entry content url {{{1
################################################################

ChemspiderConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {

	token.param <- if (is.na(.self$getToken())) '' else paste('&token', .self$getToken(), sep = '=')
	if (concatenate)
		url <- paste0(.self$getBaseUrl(), 'MassSpecAPI.asmx/GetExtendedCompoundInfoArray?', paste(paste0('CSIDs=', id), collapse = '&'), token.param)
	else
		url <- paste0(.self$getBaseUrl(), 'MassSpecAPI.asmx/GetExtendedCompoundInfoArray?CSIDs=', id, token.param)

	return(url)
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

# Web service filter-mass-post {{{1
################################################################

ChemspiderConn$methods( ws.filterNamePost = function(name, retfmt = c('plain', 'parsed', 'queryid', 'ids')) {
	":\n\nAccess the filter-name-post ChemSpider web service. See https://developer.rsc.org/compounds-v1/apis/post/filter/name."

	retfmt <- match.arg(retfmt)

	# Set URL
	url <- paste0(.self$getUrl('ws.url'), 'filter/name')

	# Set header
	header <- c('Content-Type' = "", apikey = .self$getToken())

	# Set body
	body <- paste0("{\n", '\t"name": "', name, '"', "\n}")

	# Send request
	results <- .self$getBiodb()$getRequestScheduler()$getUrl(url = url, method = 'post', header = header, body = body)

	# Error
	if (is.null(results) || is.na(results))
		results <- NULL
	else
		results <- .self$.retrieveQuery(results = results, retfmt = retfmt)

	return(results)
})

# Web service filter-mass-post {{{1
################################################################

ChemspiderConn$methods( ws.filterMassPost = function(mass, range, retfmt = c('plain', 'parsed', 'queryid', 'ids')) {
	":\n\nAccess the filter-mass-post ChemSpider web service. See https://developer.rsc.org/compounds-v1/apis/post/filter/mass."

	retfmt <- match.arg(retfmt)

	# Set URL
	url <- paste0(.self$getUrl('ws.url'), 'filter/mass')

	# Set header
	header <- c('Content-Type' = "", apikey = .self$getToken())

	# Set body
	body <- paste0("{\n", '\t"mass": ', mass, ",\n",'\t"range": ', range, "\n}")

	# Send request
	results <- .self$getBiodb()$getRequestScheduler()$getUrl(url = url, method = 'post', header = header, body = body)

	# Error
	if (is.null(results) || is.na(results))
		results <- NULL
	else
		results <- .self$.retrieveQuery(results = results, retfmt = retfmt)

	return(results)
})

# Web service filter-queryId-status-get {{{1
################################################################

ChemspiderConn$methods( ws.filterQueryIdStatusGet = function(queryid, retfmt = c('plain', 'parsed', 'status')) {
	":\n\nAccess the filter-queryId-status-get ChemSpider web service. See https://developer.rsc.org/compounds-v1/apis/get/filter/%7BqueryId%7D/status."

	.self$.assert.not.null(queryid)
	.self$.assert.not.na(queryid)
	.self$.assert.is(queryid, 'character')

	retfmt <- match.arg(retfmt)

	# Set URL
	url <- paste0(.self$getUrl('ws.url'), 'filter/', queryid, '/status')

	# Set header
	header <- c('Content-Type' = "", apikey = .self$getToken())

	# Send request
	results <- .self$getBiodb()$getRequestScheduler()$getUrl(url = url, method = 'get', header = header)

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

ChemspiderConn$methods( ws.filterQueryIdResultsGet = function(queryid, start = 0L, count = 0L, retfmt = c('plain', 'parsed', 'ids')) {
	":\n\nAccess the filter-queryId-results-get ChemSpider web service. See https://developer.rsc.org/compounds-v1/apis/get/filter/%7BqueryId%7D/results,"

	.self$.assert.not.null(queryid)
	.self$.assert.not.na(queryid)
	.self$.assert.is(queryid, 'character')
	.self$.assert.number(start, negative = FALSE, float.allowed = FALSE)
	.self$.assert.number(count, negative = FALSE, float.allowed = FALSE)

	retfmt <- match.arg(retfmt)

	# Set URL
	url <- paste0(.self$getUrl('ws.url'), 'filter/', queryid, '/results')

	# Set params
	params <- list()
	if (start > 0)
		params <- c(params, start = start)
	if (count > 0)
		params <- c(params, count = count)

	# Set header
	header <- c('Content-Type' = "", apikey = .self$getToken())

	# Send request
	results <- .self$getBiodb()$getRequestScheduler()$getUrl(url = url, method = 'get', header = header, params = params)

	# Parse JSON
	if (retfmt != 'plain') {

		results <- jsonlite::fromJSON(results, simplifyDataFrame = FALSE)

		# Parse IDs
		if (retfmt == 'ids')
			results <- results$rsults
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
