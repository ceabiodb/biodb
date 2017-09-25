# vi: fdm=marker

#' @include CompounddbConn.R
#' @include RemotedbConn.R

# Class declaration {{{1
################################################################

ChebiConn <- methods::setRefClass("ChebiConn", contains = c("RemotedbConn", "CompounddbConn"))

# Constructor {{{1
################################################################

ChebiConn$methods( initialize = function(...) {
	callSuper(content.type = BIODB.XML, base.url = 'https://www.ebi.ac.uk/', ...)
})

# Get entry content url {{{1
################################################################

ChebiConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {
	                  # TODO Return an URL request object with SOAP message embedded
	return(paste(.self$.base.url, 'webservices/chebi/2.0/test/getCompleteEntity?chebiId=', id, sep = ''))
})

# Get entry content {{{1
################################################################

ChebiConn$methods( getEntryContent = function(entry.id) {

	# Initialize return values
	content <- rep(NA_character_, length(entry.id))

	# Request
	content <- vapply(entry.id, function(x) .self$.getUrlScheduler()$getUrl(.self$getEntryContentUrl(x)), FUN.VALUE = '')

	return(content)
})

# Web service getLiteEntity {{{1
################################################################

ChebiConn$methods( ws.getLiteEntity = function(search = NULL, search.category = 'ALL', max.results = 10, stars = 'ALL') {

	# Check parameters
	.self$.assert.not.null(search)
	.self$.assert.not.na(search)
	.self$.assert.in(search.category, c('ALL', 'CHEBI ID', 'CHEBI NAME', 'DEFINITION', 'ALL NAMES', 'IUPAC NAME', 'DATABASE LINK/REGISTRY NUMBER/CITATION', 'FORMULA', 'MASS', 'MONOISOTOPIC MASS', 'CHARGE', 'INCHI/INCHI KEY', 'SMILES', 'SPECIES')) # TODO: could be read from WSDL http://www.ebi.ac.uk/webservices/chebi/2.0/webservice?wsdl
	if (is.na(max.results))
		max.results <- 0
	.self$.assert.positive(max.results)
	.self$.assert.in(stars, c('ALL', 'TWO ONLY', 'THREE ONLY'))

	# Build request
	xml.request <- paste0("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:tns=\"http://www.ebi.ac.uk/webservices/chebi\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/wsdl/soap/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><SOAP-ENV:Body><tns:getLiteEntity xmlns:tns=\"http://www.ebi.ac.uk/webservices/chebi\"><tns:search>", search, "</tns:search><tns:searchCategory>", search.category, "</tns:searchCategory><tns:maximumResults>", max.results, "</tns:maximumResults><tns:stars>", stars, "</tns:stars></tns:getLiteEntity></SOAP-ENV:Body></SOAP-ENV:Envelope>")

	# Send request
	xml.results <- .self$.getUrlScheduler()$sendSoapRequest('http://www.ebi.ac.uk:80/webservices/chebi/2.0/webservice', xml.request)

	# Set XML namespace
	ns <- c(chebi = "http://www.ebi.ac.uk/webservices/chebi")

	# Parse XML
	xml <-  XML::xmlInternalTreeParse(xml.results, asText = TRUE)

	# Get elements
	ids <- XML::xpathSApply(xml, "//chebi:chebiId", XML::xmlValue, namespaces = ns)
	ids <- sub('CHEBI:', '', ids)

	return(ids)
})

# Get entry ids {{{1
################################################################

ChebiConn$methods( getEntryIds = function(max.results = NA_integer_) {
	return(.self$ws.getLiteEntity(search = '1*', search.category = 'CHEBI ID', max.results = max.results))
})

# Search compound {{{1
################################################################

ChebiConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.tol = 1, mass.tol.unit = 'plain', max.results = NA_integer_) {

	id <- NULL
	
	# Search by name
	if ( ! is.null(name))
		id <- .self$ws.getLiteEntity(search = name, search.category = "ALL NAMES", max.results = 0)

	# Search by mass
	if ( ! is.null(mass)) {
		mass.ids <- .self$ws.getLiteEntity(search = mass, search.category = "MASS", max.results = 0)

		if (is.null(id))
			id <- mass.ids
		else
			id <- id[id %in% mass.ids]
	}

	if (is.null(id))
		id <- character(0)

	# Cut
	if ( ! is.na(max.results) && max.results > 0 && max.results < length(id))
		id <- id[1:max.results]

	return(id)
})
