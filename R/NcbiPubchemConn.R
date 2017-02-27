# vi: fdm=marker

# Class declaration {{{1
################################################################

NcbiPubchemConn <- methods::setRefClass("NcbiPubchemConn", contains = "RemotedbConn", fields = list(.db.name = 'character', .id.xmltag = 'character', .entry.xmltag = 'character', .id.urlfield = 'character'))

# Constructor {{{1
################################################################

NcbiPubchemConn$methods( initialize = function(db.name, id.xmltag, entry.xmltag, id.urlfield, ...) {
	callSuper(...)

	.db.name <<- db.name
	.id.xmltag <<- id.xmltag
	.entry.xmltag <<- entry.xmltag
	.id.urlfield <<- id.urlfield
})

# Do get entry content url {{{1
################################################################

NcbiPubchemConn$methods( .doGetEntryContentUrl = function(id) {
	return(paste0('https://pubchem.ncbi.nlm.nih.gov/rest/pug/', .self$.db.name, '/', .self$.id.urlfield, '/', paste(id, collapse = ','), '/XML'))
})

# Get entry page url {{{1
################################################################

NcbiPubchemConn$methods( getEntryPageUrl = function(id) {
	return(paste0('http://pubchem.ncbi.nlm.nih.gov/', .self$.db.name, '/', id))
})

# Get entry content {{{1
################################################################

NcbiPubchemConn$methods( getEntryContent = function(ids) {

	# Debug
	.self$message(MSG.INFO, paste0("Get entry content(s) for ", length(ids)," id(s)..."))

	# Get URL requests
	url.requests <- .self$getEntryContentUrl(ids, max.length = 2048)

	# Initialize return values
	content <- rep(NA_character_, length(ids))

	# Loop on all URLs
	for (url in url.requests) {

		# Send request
		xmlstr <- .self$.getUrlScheduler()$getUrl(url)

		if ( ! is.na(xmlstr) && length(grep('PUGREST.BadRequest', xmlstr)) == 0) {

			# Parse XML
			xml <-  XML::xmlInternalTreeParse(xmlstr, asText = TRUE)

			# TODO When one of the id is wrong, no content is returned. Only a single error is returned, with the first faulty ID:
	#		<Fault xmlns="http://pubchem.ncbi.nlm.nih.gov/pug_rest" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xs:schemaLocation="http://pubchem.ncbi.nlm.nih.gov/pug_rest https://pubchem.ncbi.nlm.nih.gov/pug_rest/pug_rest.xsd">
	#		<Code>PUGREST.NotFound</Code>
	#		<Message>Record not found</Message>
	#		<Details>No record data for CID 1246452553</Details>
	#		</Fault>

			# Get returned IDs
			ns <- c(pcns = "http://www.ncbi.nlm.nih.gov")
			returned.ids <- XML::xpathSApply(xml, paste0("//pcns:", .self$.id.xmltag), XML::xmlValue, namespaces = ns)

			# Store contents
			content[match(returned.ids, ids)] <- vapply(XML::getNodeSet(xml, paste0("//pcns:", .self$.entry.xmltag), namespaces = ns), XML::saveXML, FUN.VALUE = '')
		}

	}

	.self$message(MSG.DEBUG, paste("Returning found content (nchars = ", paste(nchar(content), collapse = ", "), ")."))

	return(content)
})

# Get compound image url {{{1
################################################################

NcbiPubchemConn$methods( getCompoundImageUrl = function(id) {

	url <- paste0('http://pubchem.ncbi.nlm.nih.gov/image/imgsrv.fcgi?', .self$.id.urlfield, '=', id, '&t=l')

	return(url)
})
