# vi: fdm=marker

# Class declaration {{{1
################################################################

MassbankConn <- methods::setRefClass("MassbankConn", contains = c("RemotedbConn", "MassdbConn"))

# Constructor {{{1
################################################################

MassbankConn$methods( initialize = function(url = NA_character_, ...) {

	callSuper(content.type = BIODB.TXT, base.url = .self$getBiodb()$getConfig()$get(CFG.MASSBANK.URL), ...)
})

# Send URL request {{{1
################################################################

MassbankConn$methods( .send.url.request = function(url) {

	# Send request
	result <- .self$.getUrlScheduler()$getUrl(url)

	# Test if a server error occured
	if (length(grep("The service cannot be found", result)) > 0)
		.self$message(MSG.ERROR, paste("Massbank website \"", .self$getBaseUrl(), "\" is not available.", sep = ''))

	return(result)
})

# Get entry content {{{1
################################################################

MassbankConn$methods( getEntryContent = function(id) {

	# Debug
	.self$message(MSG.DEBUG, paste0("Get entry content(s) for ", length(id)," id(s)..."))

	URL.MAX.LENGTH <- 2083

	# Initialize return values
	content <- rep(NA_character_, length(id))

	# Get URLs
	urls <- .self$getEntryContentUrl(id, max.length = URL.MAX.LENGTH)

	# Loop on all URLs
	for (url in urls) {

		# Send request
		xmlstr <- .self$.send.url.request(url)

		# Parse XML and get text
		if ( ! is.na(xmlstr)) {
			xml <-  XML::xmlInternalTreeParse(xmlstr, asText = TRUE)
			ns <- c(ax21 = "http://api.massbank/xsd")
			returned.ids <- XML::xpathSApply(xml, "//ax21:id", XML::xmlValue, namespaces = ns)
			if (length(returned.ids) > 0)
				content[match(returned.ids, id)] <- XML::xpathSApply(xml, "//ax21:info", XML::xmlValue, namespaces = ns)
		}
	}

	return(content)
})

# Create entry {{{1
################################################################

# Creates a Spectrum instance from file content.
# content       A file content, downloaded from the public database.
# RETURN        A spectrum instance.
MassbankConn$methods( createEntry = function(content, drop = TRUE) {
	return(createMassbankEntryFromTxt(.self$getBiodb(), content, drop = drop))
})

# Get mz values {{{1
################################################################

MassbankConn$methods( getMzValues = function(mode = NULL, max.results = NA_integer_) {
})

# Do search peak {{{1
################################################################

MassbankConn$methods( .do.search.peak = function(mz = NA_real_, tol = NA_real_, relint = 100, mode = NA_character_, max.results = NA_integer_) {

	# Set URL
	url <- paste0(.self$getBaseUrl(), 'searchPeak?mzs=', mz)
	url <- paste0(url, '&relativeIntensity=', if (is.na(relint)) 0 else relint)
	url <- paste0(url, '&tolerance=', tol, '&instrumentTypes=all')
	url <- paste0(url, '&ionMode=', if (is.na(mode)) 'Both' else ( if (mode == BIODB.MSMODE.NEG) 'Negative' else 'Positive'))
	url <- paste0(url, '&maxNumResults=', (if (is.na(max.results)) 0 else max.results))

	# Send request
	xmlstr <- .self$.send.url.request(url)

	# Parse XML and get text
	if ( ! is.na(xmlstr)) {
		xml <-  XML::xmlInternalTreeParse(xmlstr, asText = TRUE)
		ns <- c(ax21 = "http://api.massbank/xsd")
		returned.ids <- XML::xpathSApply(xml, "//ax21:id", XML::xmlValue, namespaces = ns)
		return(returned.ids)
	}

})

# Get entry ids {{{1
################################################################

MassbankConn$methods( getEntryIds = function(max.results = NA_integer_) {

	.self$message(MSG.INFO, paste("Getting", if (is.na(max.results)) 'all' else max.results, "massbank entry ids..."))

	return(.self$searchPeak(mz = 1000, tol = 1000, relint = 100, max.results = max.results))
})

# Get nb entries {{{1
################################################################

MassbankConn$methods( getNbEntries = function(count = FALSE) {
	return(if (count) length(.self$getEntryIds()) else NA_integer_)
})

# Do get entry content url {{{1
################################################################

MassbankConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {

	if (concatenate)
		url <- paste(.self$getBaseUrl(), 'getRecordInfo?ids=', paste(id, collapse = ','), sep = '')
	else
		url <- paste(.self$getBaseUrl(), 'getRecordInfo?ids=', id, sep = '')

	return(url)
})
