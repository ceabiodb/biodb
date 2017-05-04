# vi: fdm=marker

#' @include MassbankConn.R

# Constants {{{1
################################################################

MASSBANK.EU.URL  <- 'http://massbank.eu/'

# Class declaration {{{1
################################################################

MassbankEuConn <- methods::setRefClass("MassbankEuConn", contains = 'MassbankConn')

# Constructor {{{1
################################################################0

MassbankEuConn$methods( initialize = function(...) {

	callSuper(base.url = MASSBANK.EU.URL, ...)
})

# Get entry content {{{1
################################################################

MassbankEuConn$methods( getEntryContent = function(id) {

	# Debug
	.self$message(MSG.DEBUG, paste0("Get entry content(s) for ", length(id)," id(s)..."))

	# Initialize return values
	content <- rep(NA_character_, length(id))

	# Build request
	xml.request <- paste0('<?xml version="1.0" encoding="UTF-8"?><SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:tns="http://api.massbank"><SOAP-ENV:Body><tns:getRecordInfo>', paste(paste('<tns:ids>', id, '</tns:ids>', sep = ''), collapse = ''), '</tns:getRecordInfo></SOAP-ENV:Body></SOAP-ENV:Envelope>')

	# Send request
	xmlstr <- .self$.scheduler$sendSoapRequest(paste0(.self$getBaseUrl(), 'api/services/MassBankAPI.MassBankAPIHttpSoap11Endpoint/'), xml.request)

	# Parse XML and get text
	if ( ! is.na(xmlstr)) {
		xml <-  XML::xmlInternalTreeParse(xmlstr, asText = TRUE)
		ns <- c(ax21 = "http://api.massbank/xsd")
		returned.ids <- XML::xpathSApply(xml, "//ax21:id", XML::xmlValue, namespaces = ns)
		if (length(returned.ids) > 0)
			content[match(returned.ids, id)] <- XML::xpathSApply(xml, "//ax21:info", XML::xmlValue, namespaces = ns)
	}

	return(content)
})

# Get entry ids {{{1
################################################################

MassbankEuConn$methods( getEntryIds = function(max.results = NA_integer_) {
	ids <- .self$searchMzRange(10, 1000, max.results = max.results)
	return(ids)
})
