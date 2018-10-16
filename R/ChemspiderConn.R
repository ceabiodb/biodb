# vi: fdm=marker

# Class declaration {{{1
################################################################

#' The connector class to ChemSpider database.
#'
#' This is a concrete connector class. It must never be instantiated directly, but instead be instantiated through the factory \code{\link{BiodbFactory}}. Only specific methods are described here. See super classes for the description of inherited methods.
#'
#' @param mass  The mass to search for.
#' @param query The query to send to the database.
#' @param range The range of the searched mass. Plain range, Dalton unit. The mass searched are between (mass - range) and (mass + range).
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{RemotedbConn}}, \code{\link{CompounddbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
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
			.self$message('info', 'ZAP')

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
				returned.ids <- XML::xpathSApply(xml, "//ns:ExtendedCompoundInfo/ns:CSID", XML::xmlValue, namespaces = c(ns = .self$getDbInfo()$getXmlNs()))
				content[match(returned.ids, entry.id)] <- vapply(XML::getNodeSet(xml, "//ns:ExtendedCompoundInfo", namespaces = c(ns = .self$getDbInfo()$getXmlNs())), XML::saveXML, FUN.VALUE = '')
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

## Send search mass request {{{1
#################################################################
#
#ChemspiderConn$methods( .send.search.mass.request = function(mass, range) {
#	"!!! PRIVATE METHOD !!! Send a \"search mass\" request, and get the ID of the open transaction.
#	mass:   The mass to search.
#	range:  ???
#	return: The transaction ID."
#
#	# Build request
#	xml.request <- paste('<?xml version="1.0" encoding="utf-8"?>
#		<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
#			<soap:Body>
#				<SearchByMassAsync xmlns="http://www.chemspider.com/">
#					<mass>', mass, '</mass>
#					<range>', range, '</range>
#					<token>', .self$getToken(), '</token>
#				</SearchByMassAsync>
#			</soap:Body>
#		</soap:Envelope>', sep = '')
#
#	# Send request
#	.self$message('debug', paste("XML REQUEST =", xml.request))
#	xml.results <- .self$.getUrlScheduler()$sendSoapRequest(paste(.self$getBaseUrl(), "MassSpecAPI.asmx", sep = ''), soap.action = paste(.self$getBaseUrl(), "SearchByMassAsync", sep = ''), soap.request = xml.request)
#	.self$message('debug', paste("XML RESULTS =", xml.results))
#
#	# Parse XML
#	xml <-  XML::xmlInternalTreeParse(xml.results, asText = TRUE)
#
#	# Get transaction ID
#	id <- XML::xpathSApply(xml, "//chemspider:SearchByMassAsyncResult", XML::xmlValue, namespaces = c(chemspider = .self$getDbInfo()$getXmlNs()))
#	.self$message('debug', paste("Transaction ID = ", id, ".", sep = ''))
#
#	return(id)
#})

# Web service SearchByMass2 {{{1
################################################################

ChemspiderConn$methods( ws.SearchByMass2 = function(mass = NA, range = NA) {
	":\n\nDirect query to the database for searching for compounds by monoisotopic mass. See http://www.chemspider.com/MassSpecAPI.asmx?op=SearchByMass2 for details."

	xml.results <- .self$.getUrlScheduler()$getUrl(paste(.self$getBaseUrl(), "MassSpecAPI.asmx/SearchByMass2", sep = ''), params = c(mass = mass, range = range))

	return(xml.results)
})

# Web service SearchByMass2 IDs {{{1
################################################################

ChemspiderConn$methods( ws.SearchByMass2.ids = function(...) {
	":\n\nCalls ws.SearchByMass2() but only for getting IDs. Returns the IDs as a character vector."

	results <- .self$ws.SearchByMass2(...)

	# Parse XML
	xml <-  XML::xmlInternalTreeParse(results, asText = TRUE)

	# Get IDs
	id <- XML::xpathSApply(xml, "/chemspider:ArrayOfString/chemspider:string", XML::xmlValue, namespaces = c(chemspider = .self$getDbInfo()$getXmlNs()))

	return(id)
})

# Web service SimpleSearch {{{1
################################################################

ChemspiderConn$methods( ws.SimpleSearch = function(query = NA) {
	":\n\nDirect query to the database for searching for compounds by name, SMILES, InChI, InChIKey, etc.. See http://www.chemspider.com/Search.asmx?op=SimpleSearch for details."
	xml.results <- .self$.getUrlScheduler()$getUrl(paste(.self$getBaseUrl(), "Search.asmx/SimpleSearch", sep = ''), params = c(query = query, token = .self$getToken()))

	return(xml.results)
})

# Web service SimpleSearch IDs {{{1
################################################################

ChemspiderConn$methods( ws.SimpleSearch.ids = function(...) {
	":\n\nCalls ws.SimpleSearch() but only for getting IDs. Returns the IDs as a character vector."

	results <- .self$ws.SimpleSearch(...)

	# Parse XML
	xml <-  XML::xmlInternalTreeParse(results, asText = TRUE)

	# Get IDs
	id <- XML::xpathSApply(xml, "/chemspider:ArrayOfInt/chemspider:int", XML::xmlValue, namespaces = c(chemspider = .self$getDbInfo()$getXmlNs()))
	id <- as.character(id)

	return(id)
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
			ids <- .self$ws.SearchByMass2.ids(mass = mass, range = range)
		}
		else
			.self$message('caution', paste0('Mass field "', mass.field, '" is not handled.'))
	}

	# Search by name
	if ( ! is.null(name)) {

		name.id <- .self$ws.SimpleSearch.ids(query = name)

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
