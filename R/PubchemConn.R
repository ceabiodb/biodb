#####################
# CLASS DECLARATION #
#####################

PubchemConn <- methods::setRefClass("PubchemConn", contains = "RemotedbConn", fields = list( .db = "character" ))

###############
# CONSTRUCTOR #
###############

PubchemConn$methods( initialize = function(db = BIODB.PUBCHEMCOMP, ...) {
	callSuper(content.type = BIODB.XML, ...)
	.db <<- db
})

#####################
# GET ENTRY CONTENT #
#####################

PubchemConn$methods( getEntryContent = function(ids) {

	# Debug
	.self$message(MSG.INFO, paste0("Get entry content(s) for ", length(ids)," id(s)..."))

	URL.MAX.LENGTH <- 2083

	# Initialize return values
	content <- rep(NA_character_, length(ids))

	# Loop on all
	n <- 0
	while (n < length(ids)) {

		# Get list of accession ids to retrieve
		accessions <- ids[(n + 1):length(ids)]

		# Create URL request
		x <- get.entry.url(class = .self$.db, accession = accessions, content.type = BIODB.XML, max.length = URL.MAX.LENGTH)

		# Debug
		.self$message(MSG.INFO, paste0("Send URL request for ", x$n," id(s)..."))

		# Send request
		xmlstr <- .self$.get.url(x$url)

		# Increase number of entries retrieved
		n <- n + x$n

		# TODO When one of the id is wrong, no content is returned. Only a single error is returned, with the first faulty ID:
#		<Fault xmlns="http://pubchem.ncbi.nlm.nih.gov/pug_rest" xmlns:xs="http://www.w3.org/2001/XMLSchema-instance" xs:schemaLocation="http://pubchem.ncbi.nlm.nih.gov/pug_rest https://pubchem.ncbi.nlm.nih.gov/pug_rest/pug_rest.xsd">
#		<Code>PUGREST.NotFound</Code>
#		<Message>Record not found</Message>
#		<Details>No record data for CID 1246452553</Details>
#		</Fault>

		# Parse XML and get included XML
		if ( ! is.na(xmlstr)) {
			xml <-  XML::xmlInternalTreeParse(xmlstr, asText = TRUE)
			ns <- c(pcns = "http://www.ncbi.nlm.nih.gov")
			returned.ids <- XML::xpathSApply(xml, paste0("//pcns:", if (.self$.db == BIODB.PUBCHEMCOMP) 'PC-CompoundType_id_cid' else 'PC-ID_id'), XML::xmlValue, namespaces = ns)
			content[match(returned.ids, ids)] <- vapply(XML::getNodeSet(xml, paste0("//pcns:", if (.self$.db == BIODB.PUBCHEMCOMP) "PC-Compound" else 'PC-Substance'), namespaces = ns), XML::saveXML, FUN.VALUE = '')
		}

		# Debug
		.self$message(MSG.INFO, paste0("Now ", length(ids) - n," id(s) left to be retrieved..."))
	}

	return(content)
})

################
# CREATE ENTRY #
################

PubchemConn$methods( createEntry = function(content, drop = TRUE) {
	return(if (.self$.db == BIODB.PUBCHEMCOMP) createPubchemEntryFromXml(.self$getBiodb(), content, drop = drop) else createPubchemSubstanceFromXml(.self$getBiodb(), content, drop = drop))
})

#########################
# GET PUBCHEM IMAGE URL #
#########################

get.pubchem.image.url <- function(id, db = BIODB.PUBCHEMCOMP) {

	url <- paste0('http://pubchem.ncbi.nlm.nih.gov/image/imgsrv.fcgi?', (if (db == BIODB.PUBCHEMCOMP) 'cid' else 'sid'), '=', id, '&t=l')

	return(url)
}
