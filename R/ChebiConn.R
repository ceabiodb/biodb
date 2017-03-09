# vi: fdm=marker

# Class declaration {{{1
################################################################

ChebiConn <- methods::setRefClass("ChebiConn", contains = "RemotedbConn")

# Constructor {{{1
################################################################

ChebiConn$methods( initialize = function(...) {
	callSuper(content.type = BIODB.XML, base.url = 'https://www.ebi.ac.uk/', ...)
})

# Get entry content url {{{1
################################################################

ChebiConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {
	return(paste(.self$.base.url, 'webservices/chebi/2.0/test/getCompleteEntity?chebiId=', id, sep = ''))
})

# Get entry content {{{1
################################################################

ChebiConn$methods( getEntryContent = function(id) {

	# Initialize return values
	content <- rep(NA_character_, length(id))

	# Request
	content <- vapply(id, function(x) .self$.getUrlScheduler()$getUrl(.self$getEntryContentUrl(x)), FUN.VALUE = '')

	return(content)
})


# Get entry ids {{{1
################################################################

ChebiConn$methods( getEntryIds = function(max.results = NA_integer_) {

	# Build request
	xml.request <- paste0("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:tns=\"http://www.ebi.ac.uk/webservices/chebi\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/wsdl/soap/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><SOAP-ENV:Body><tns:getLiteEntity xmlns:tns=\"http://www.ebi.ac.uk/webservices/chebi\"><tns:search>1*</tns:search><tns:searchCategory>CHEBI ID</tns:searchCategory><tns:maximumResults>", max.results, "</tns:maximumResults><tns:stars></tns:stars></tns:getLiteEntity></SOAP-ENV:Body></SOAP-ENV:Envelope>")

	# Send request
	xml.results <- .self$.scheduler$sendSoapRequest('http://www.ebi.ac.uk:80/webservices/chebi/2.0/webservice', xml.request)

	# Set XML namespace
	ns <- c(chebi = "http://www.ebi.ac.uk/webservices/chebi")

	# Parse XML
	xml <-  XML::xmlInternalTreeParse(xml.results, asText = TRUE)

	# Get elements
	ids <- XML::xpathSApply(xml, "//chebi:chebiId", XML::xmlValue, namespaces = ns)
	ids <- sub('CHEBI:', '', ids)

	return(ids)
})
