#####################
# CLASS DECLARATION #
#####################

ChebiConn <- methods::setRefClass("ChebiConn", contains = "RemotedbConn")

##########################
# GET ENTRY CONTENT TYPE #
##########################

ChebiConn$methods( getEntryContentType = function() {
	return(BIODB.HTML)
})

#####################
# GET ENTRY CONTENT #
#####################

ChebiConn$methods( getEntryContent = function(id) {

	# Initialize return values
	content <- rep(NA_character_, length(id))

	# Request
	content <- vapply(id, function(x) .self$.get.url(get.entry.url(BIODB.CHEBI, x)), FUN.VALUE = '')

	return(content)
})

################
# CREATE ENTRY #
################

ChebiConn$methods( createEntry = function(content, drop = TRUE) {
	return(createChebiEntryFromHtml(content, drop = drop))
})

##################
# GET NB ENTRIES #
##################

ChebiConn$methods( getNbEntries = function() {
	return(NA_integer_)
})

#################
# GET ENTRY IDS #
#################

ChebiConn$methods( getEntryIds = function(max.results = NA_integer_) {
	request <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:tns=\"http://www.ebi.ac.uk/webservices/chebi\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soap=\"http://schemas.xmlsoap.org/wsdl/soap/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" ><SOAP-ENV:Body><tns:getLiteEntity xmlns:tns=\"http://www.ebi.ac.uk/webservices/chebi\"><tns:search>1*</tns:search><tns:searchCategory>CHEBI ID</tns:searchCategory><tns:maximumResults>100</tns:maximumResults><tns:stars></tns:stars></tns:getLiteEntity></SOAP-ENV:Body></SOAP-ENV:Envelope>"
	results <- .self$.scheduler$sendSoapRequest('http://www.ebi.ac.uk:80/webservices/chebi/2.0/webservice', request)
	.self$message(MSG.DEBUG, paste0("Received response from ChEBI: ", substr(results, 0, 10)))
	return(NULL)
})
