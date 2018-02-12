# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include CompounddbConn.R
#' @include RemotedbConn.R
ChebiConn <- methods::setRefClass("ChebiConn", contains = c("RemotedbConn", "CompounddbConn"))

# Get entry content url {{{1
################################################################

ChebiConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {
	return(paste(file.path(.self$getWsUrl(), 'getCompleteEntity', fsep = '/'), '?chebiId=', id, sep = ''))
})

# Get entry page url {{{1
################################################################

ChebiConn$methods( getEntryPageUrl = function(id) {
	return(paste0(.self$getBaseUrl(), 'searchId.do?chebiId=', id))
})

# Get entry image url {{{1
################################################################

ChebiConn$methods( getEntryImageUrl = function(id) {
	return(paste0(.self$getBaseUrl(), 'displayImage.do?defaultImage=true&imageIndex=0&chebiId=', id, '&dimensions=400'))
})

# Get entry content {{{1
################################################################

ChebiConn$methods( getEntryContent = function(entry.id) {

	# Initialize return values
	content <- rep(NA_character_, length(entry.id))

	# Request
	content <- vapply(entry.id, function(x) .self$.getUrlScheduler()$getUrl(.self$getEntryContentUrl(x), encoding = 'UTF-8'), FUN.VALUE = '')

	return(content)
})

# Web service getLiteEntity {{{1
################################################################

ChebiConn$methods( ws.getLiteEntity = function(search = NULL, search.category = 'ALL', max.results = 10, stars = 'ALL') {
	":\n\nCalls getLiteEntity web service and returns the XML result. See http://www.ebi.ac.uk/chebi/webServices.do. Be careful when search by mass (search.category = 'MASS' or 'MONOISOTOPIC MASS', since the searched is made in text mode, thus the number must be exactly written as it stored in database eventually padded with 0 in order to have exactly 5 digits after the decimal. An easy solution is to use wildcards to search a mass: '410;.718*'."

	# Check parameters
	.self$.assert.not.null(search)
	.self$.assert.not.na(search)
	.self$.assert.in(search.category, c('ALL', 'CHEBI ID', 'CHEBI NAME', 'DEFINITION', 'ALL NAMES', 'IUPAC NAME', 'DATABASE LINK/REGISTRY NUMBER/CITATION', 'FORMULA', 'MASS', 'MONOISOTOPIC MASS', 'CHARGE', 'INCHI/INCHI KEY', 'SMILES', 'SPECIES')) # TODO: could be read from WSDL http://www.ebi.ac.uk/webservices/chebi/2.0/webservice?wsdl
	if (is.na(max.results))
		max.results <- 0
	.self$.assert.positive(max.results)
	.self$.assert.in(stars, c('ALL', 'TWO ONLY', 'THREE ONLY'))

	# Set request parameters
	params <- c(search = gsub('[ /]', '+', search), searchCategory = gsub(' ', '+', search.category), maximumResults = max.results, starsCategory = gsub(' ', '+', stars))

	# Send request
	xml.results <- .self$.getUrlScheduler()$getUrl(file.path(.self$getWsUrl(), 'getLiteEntity', fsep = '/'), params = params, encoding = 'UTF-8')

	return(xml.results)
})

# Web service getLiteEntity IDs {{{1
################################################################

ChebiConn$methods( ws.getLiteEntity.ids = function(...) {
	":\n\nCalls ws.getLiteEntity() but only for getting IDs. Returns the IDs as a character vector."

	xml.results <- .self$ws.getLiteEntity(...)

	# Parse XML
	xml <-  XML::xmlInternalTreeParse(xml.results, asText = TRUE)

	# Get elements
	ids <- XML::xpathSApply(xml, "//ns:chebiId", XML::xmlValue, namespaces = c(ns = .self$getDbInfo()$getXmlNs()))
	ids <- sub('CHEBI:', '', ids)
	if (length(grep("^[0-9]+$", ids)) != length(ids))
		.self$message('error', paste("Impossible to parse XML to get entry IDs:\n", xml.results))

	return(ids)
})

# Get entry ids {{{1
################################################################

ChebiConn$methods( getEntryIds = function(max.results = NA_integer_) {
	return(.self$ws.getLiteEntity.ids(search = '1*', search.category = 'CHEBI ID', max.results = max.results))
})

# Search compound {{{1
################################################################

ChebiConn$methods( searchCompound = function(name = NULL, molecular.mass = NULL, monoisotopic.mass = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {

	ids <- NULL
	
	# Search by name
	if ( ! is.null(name))
		ids <- .self$ws.getLiteEntity.ids(search = name, search.category = "ALL NAMES", max.results = 0)

	# Search by mass
	if ( ! is.null(monoisotopic.mass) || ! is.null(molecular.mass)) {

		mass <- if (is.null(monoisotopic.mass)) molecular.mass else monoisotopic.mass

		if (is.null(ids)) {
			.self$message('caution', 'ChEBI does not use any tolerance while searching for compounds by mass. Thus, only compounds matching exactly the specified mass will be matched.')
			ids <- .self$ws.getLiteEntity.ids(search = paste0(mass, '*'), search.category = (if (is.null(monoisotopic.mass)) "MASS" else "MONOISOTOPIC MASS"), max.results = 0)
		}
		else {
			.self$message('caution', 'Since ChEBI does not use tolerance while searching for compounds by mass, we will do filtering by mass directly on results obtained from the search by name.')

			if (mass.tol.unit == 'ppm') {
				mass.min <- mass * (1 - mass.tol * 1e-6)
				mass.max <- mass * (1 + mass.tol * 1e-6)
			} else {
				mass.min <- mass - mass.tol
				mass.max <- mass + mass.tol
			}

			# Get masses of all entries
			entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), ids, drop = FALSE)
			masses <- .self$getBiodb()$entriesToDataframe(entries, compute = FALSE, fields = (if (is.null(molecular.mass)) 'monoisotopic.mass' else 'molecular.mass'), drop = TRUE)

			# Filter on mass
			ids <- ids[(masses >= mass.min) & (masses <= mass.max)]
		}
	}

	if (is.null(ids))
		ids <- character(0)

	# Cut
	if ( ! is.na(max.results) && max.results > 0 && max.results < length(ids))
		ids <- ids[1:max.results]

	return(ids)
})
