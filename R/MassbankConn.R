# vi: fdm=marker

# Class declaration {{{1
################################################################

MassbankConn <- methods::setRefClass("MassbankConn", contains = c("RemotedbConn", "MassdbConn"), fields = list( .url = "character" ))

# Constructor {{{1
################################################################

MassbankConn$methods( initialize = function(url = NA_character_, ...) {

	callSuper(content.type = BIODB.TXT, ...)

	# Set URL
	.url <<- if (is.null(url) || is.na(url)) BIODB.MASSBANK.EU.WS.URL else url
})

# Get entry content {{{1
################################################################

MassbankConn$methods( getEntryContent = function(ids) {

	# Debug
	.self$message(MSG.DEBUG, paste0("Get entry content(s) for ", length(ids)," id(s)..."))

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
		.self$message(MSG.INFO, paste0("Send URL request for ", x$n," id(s)..."))

		# Send request
		xmlstr <- .self$.get.url(x$url)

		# Increase number of entries retrieved
		n <- n + x$n

		# Parse XML and get text
		if ( ! is.na(xmlstr)) {
			xml <-  XML::xmlInternalTreeParse(xmlstr, asText = TRUE)
			ns <- c(ax21 = "http://api.massbank/xsd")
			returned.ids <- XML::xpathSApply(xml, "//ax21:id", XML::xmlValue, namespaces = ns)
			if (length(returned.ids) > 0)
				content[match(returned.ids, ids)] <- XML::xpathSApply(xml, "//ax21:info", XML::xmlValue, namespaces = ns)
		}

		# Debug
		.self$message(MSG.INFO, paste0("Now ", length(ids) - n," id(s) left to be retrieved..."))
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
	url <- paste0(.self$.url, 'searchPeak?mzs=', mz)
	url <- paste0(url, '&relativeIntensity=', if (is.na(relint)) 0 else relint)
	url <- paste0(url, '&tolerance=', tol, '&instrumentTypes=all')
	url <- paste0(url, '&ionMode=', if (is.na(mode)) 'Both' else ( if (mode == BIODB.MSMODE.NEG) 'Negative' else 'Positive'))
	url <- paste0(url, '&maxNumResults=', (if (is.na(max.results)) 0 else max.results))

	# Send request
	xmlstr <- .self$.get.url(url)

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
