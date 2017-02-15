# vi: fdm=marker

# Class declaration {{{1
################################################################

ChemspiderConn <- methods::setRefClass("ChemspiderConn", contains = c("RemotedbConn", "CompounddbConn"), fields = list(.ns = "character"))

# Constructor {{{1
################################################################

ChemspiderConn$methods( initialize = function(...) {

	callSuper(content.type = BIODB.XML, base.url = "http://www.chemspider.com/", ...)

	# Set XML namespace
	.ns <<- c(chemspider = "http://www.chemspider.com/")
})

# Get entry content {{{1
################################################################

ChemspiderConn$methods( getEntryContent = function(ids) {

	# Debug
	.self$message(MSG.INFO, paste0("Get entry content(s) for ", length(ids)," id(s)..."))

	URL.MAX.LENGTH <- 2083

	# Initialize return values
	content <- rep(NA_character_, length(ids))

	# Loop on all
	n <- 0
	inc <- NA_integer_
	while (n < length(ids)) {

		# Get list of accession ids to retrieve
		accessions <- ids[(n + 1):(if (is.na(inc)) length(ids) else (min(n + inc, length(ids))))]

		# Create URL request
		x <- get.entry.url(class = BIODB.CHEMSPIDER, accession = accessions, content.type = BIODB.XML, max.length = URL.MAX.LENGTH, base.url = .self$.url, token = .self$.token)

		# Debug
		.self$message(MSG.INFO, paste0("Send URL request for ", x$n," id(s)..."))

		# Send request
		xmlstr <- .self$.get.url(x$url)

		# Error : "Cannot convert WRONG to System.Int32.\r\nParameter name: type ---> Input string was not in a correct format.\r\n"
		if (grepl('^Cannot convert .* to System\\.Int32\\.', xmlstr)) {
			# One of the ids is incorrect
			if (is.na(inc)) {
				inc <- 1
				next
			}
			else
				xmlstr <- NA_character_
		}

		# Increase number of entries retrieved
		n <- n + x$n

		# Parse XML and get included XML
		if ( ! is.na(xmlstr)) {
			xml <-  XML::xmlInternalTreeParse(xmlstr, asText = TRUE)
			ns <- c(csns = "http://www.chemspider.com/")
			returned.ids <- XML::xpathSApply(xml, "//csns:ExtendedCompoundInfo/csns:CSID", XML::xmlValue, namespaces = ns)
			content[match(returned.ids, ids)] <- vapply(XML::getNodeSet(xml, "//csns:ExtendedCompoundInfo", namespaces = ns), XML::saveXML, FUN.VALUE = '')
		}

		# Debug
		.self$message(MSG.INFO, paste0("Now ", length(ids) - n," id(s) left to be retrieved..."))
	}

	return(content)
})

# Create entry {{{1
################################################################

ChemspiderConn$methods( createEntry = function(content, drop = TRUE) {
	return(createChemspiderEntryFromXml(.self$getBiodb(), content, drop = drop))
})

# Get chemspider image url {{{1
################################################################

get.chemspider.image.url <- function(id) {

	url <- paste0('http://www.chemspider.com/ImagesHandler.ashx?w=300&h=300&id=', id)

	return(url)
}

# Send search mass request {{{1
################################################################

ChemspiderConn$methods( .send.search.mass.request = function(mass, range) {
	"!!! PRIVATE METHOD !!! Send a \"search mass\" request, and get the ID of the open transaction.
	mass:   The mass to search.
	range:  ???
	return: The transaction ID."

	# Build request
	xml.request <- paste('<?xml version="1.0" encoding="utf-8"?>
		<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
			<soap:Body>
				<SearchByMassAsync xmlns="http://www.chemspider.com/">
					<mass>', mass, '</mass>
					<range>', range, '</range>
					<token>', .self$getToken(), '</token>
				</SearchByMassAsync>
			</soap:Body>
		</soap:Envelope>', sep = '')

	# Send request
	.self$message(MSG.DEBUG, paste("XML REQUEST =", xml.request))
	xml.results <- .self$.getUrlScheduler()$sendSoapRequest(paste(.self$getBaseUrl(), "MassSpecAPI.asmx", sep = ''), action = paste(.self$getBaseUrl(), "SearchByMassAsync", sep = ''), request = xml.request)
	.self$message(MSG.DEBUG, paste("XML RESULTS =", xml.results))

	# Parse XML
	xml <-  XML::xmlInternalTreeParse(xml.results, asText = TRUE)

	# Get transaction ID
	id <- XML::xpathSApply(xml, "//chemspider:SearchByMassAsyncResult", XML::xmlValue, namespaces = .self$.ns)
	.self$message(MSG.DEBUG, paste("Transaction ID = ", id, ".", sep = ''))

	return(id)
})

# Search entry by mass {{{1
################################################################

ChemspiderConn$methods( searchEntryByMass = function(mass, tol, max.results = NA_integer_) {

	# Send request
	xml.results <- .self$.getUrlScheduler()$getUrl(paste(.self$getBaseUrl(), "MassSpecAPI.asmx/SearchByMass2", sep = ''), params = c(mass = mass, range = tol))

	# Parse XML
	xml <-  XML::xmlInternalTreeParse(xml.results, asText = TRUE)

	# Get IDs
	id <- XML::xpathSApply(xml, "/chemspider:ArrayOfString/chemspider:string", XML::xmlValue, namespaces = .self$.ns)

	# Cut
	if ( ! is.na(max.results) && max.results > 0 && max.results < length(id))
		id <- id[1:max.results]

	return(id)
})

# Get entry ids {{{1
################################################################

ChemspiderConn$methods( getEntryIds = function(max.results = NA_integer_) {
	"This method is not correctly implemented. This is because ChemSpider API does not provide a service for obtaining the exact number of entries. As a consequence we use `searchEntryByMass()` method to search for entries. However, since looking for all entries this way makes ChemSpider fails with `System.OutOfMemoryException`, only a subset of the entries is retrieve. This method, implemented this way, is still useful for testing purposes."

	.self$message(MSG.CAUTION, "Method using a last resort solution for its implementation. Returns only a small subset of ChemSpider entries.")

	return(.self$searchEntryByMass(100, 10, max.results = max.results))
})
