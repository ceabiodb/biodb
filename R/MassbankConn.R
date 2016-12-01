#####################
# CLASS DECLARATION #
#####################

MassbankConn <- methods::setRefClass("MassbankConn", contains = c("RemotedbConn", "MassdbConn"), fields = list( .url = "character" ))

###############
# CONSTRUCTOR #
###############

MassbankConn$methods( initialize = function(url = NA_character_, ...) {

	# Set URL
	.url <<- if (is.null(url) || is.na(url)) BIODB.MASSBANK.EU.WS.URL else url

	callSuper(...)
})

##########################
# GET ENTRY CONTENT TYPE #
##########################

MassbankConn$methods( getEntryContentType = function() {
	return(BIODB.TXT) 
})

#####################
# GET ENTRY CONTENT #
#####################

MassbankConn$methods( getEntryContent = function(ids) {

	# Debug
	.self$.print.debug.msg(paste0("Get entry content(s) for ", length(ids)," id(s)..."))

	URL.MAX.LENGTH <- 2083

	# Initialize return values
	content <- rep(NA_character_, length(ids))

	# Loop on all
	n <- 0
	while (n < length(ids)) {

		# Get list of accession ids to retrieve
		accessions <- ids[(n + 1):length(ids)]

		# Create URL request
		x <- get.entry.url(class = BIODB.MASSBANK, accession = accessions, content.type = BIODB.TXT, max.length = URL.MAX.LENGTH, base.url = .self$.url)

		# Debug
		.self$.print.debug.msg(paste0("Send URL request for ", x$n," id(s)..."))

		# Send request
		xmlstr <- .self$.get.url(x$url)

		# Increase number of entries retrieved
		n <- n + x$n

		# Parse XML and get text
		if ( ! is.na(xmlstr)) {
			xml <-  xmlInternalTreeParse(xmlstr, asText = TRUE)
			ns <- c(ax21 = "http://api.massbank/xsd")
			returned.ids <- xpathSApply(xml, "//ax21:id", xmlValue, namespaces = ns)
			if (length(returned.ids) > 0)
				content[match(returned.ids, ids)] <- xpathSApply(xml, "//ax21:info", xmlValue, namespaces = ns)
		}

		# Debug
		.self$.print.debug.msg(paste0("Now ", length(ids) - n," id(s) left to be retrieved..."))
	}

	return(content)
})

################
# CREATE ENTRY #
################

# Creates a Spectrum instance from file content.
# content       A file content, downloaded from the public database.
# RETURN        A spectrum instance.
MassbankConn$methods( createEntry = function(content, drop = TRUE) {
	return(createMassbankEntryFromTxt(content, drop = drop))
})

#################
# GET MZ VALUES #
#################

MassbankConn$methods( getMzValues = function(mode = NULL, max.results = NA_integer_) {
})

#################
# GET ENTRY IDS #
#################

MassbankConn$methods( getEntryIds = function(max.results = NA_integer_) {

	# Set URL
	url <- paste0(.self$.url, 'searchPeak?mzs=1000&relativeIntensity=100&tolerance=1000&instrumentTypes=all&ionMode=Both')
	url <- paste0(url, '&maxNumResults=', (if (is.na(max.results)) 0 else max.results))

	# Send request
	xmlstr <- .self$.get.url(url)

	# Parse XML and get text
	if ( ! is.na(xmlstr)) {
		xml <-  xmlInternalTreeParse(xmlstr, asText = TRUE)
		ns <- c(ax21 = "http://api.massbank/xsd")
		returned.ids <- xpathSApply(xml, "//ax21:id", xmlValue, namespaces = ns)
		return(returned.ids)
	}
})

##################
# GET NB ENTRIES #
##################

MassbankConn$methods( getNbEntries = function() {
	return(length(.self$getEntryIds()))
})
