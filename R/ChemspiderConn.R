#####################
# CLASS DECLARATION #
#####################

ChemspiderConn <- methods::setRefClass("ChemspiderConn", contains = "RemotedbConn")

##########################
# GET ENTRY CONTENT TYPE #
##########################

ChemspiderConn$methods( getEntryContentType = function() {
	return(BIODB.XML)
})

#####################
# GET ENTRY CONTENT #
#####################

ChemspiderConn$methods( getEntryContent = function(ids) {

	# Debug
	.self$.print.debug.msg(paste0("Get entry content(s) for ", length(ids)," id(s)..."))

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
		.self$.print.debug.msg(paste0("Send URL request for ", x$n," id(s)..."))

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
			xml <-  xmlInternalTreeParse(xmlstr, asText = TRUE)
			ns <- c(csns = "http://www.chemspider.com/")
			returned.ids <- xpathSApply(xml, "//csns:ExtendedCompoundInfo/csns:CSID", xmlValue, namespaces = ns)
			content[match(returned.ids, ids)] <- vapply(getNodeSet(xml, "//csns:ExtendedCompoundInfo", namespaces = ns), saveXML, FUN.VALUE = '')
		}

		# Debug
		.self$.print.debug.msg(paste0("Now ", length(ids) - n," id(s) left to be retrieved..."))
	}

	return(content)
})

################
# CREATE ENTRY #
################

ChemspiderConn$methods( createEntry = function(content, drop = TRUE) {
	return(createChemspiderEntryFromXml(content, drop = drop))
})

############################
# GET CHEMSPIDER IMAGE URL #
############################

get.chemspider.image.url <- function(id) {

	url <- paste0('http://www.chemspider.com/ImagesHandler.ashx?w=300&h=300&id=', id)

	return(url)
}
