# vi: fdm=marker

#' @include RemotedbConn.R
#' @include MassdbConn.R

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

# Get mz values {{{1
################################################################

MassbankConn$methods( getMzValues = function(ms.mode = NA_character_, max.results = NA_integer_) {

	mz <- numeric(0)

	# Get list of spectra
	spectra.ids <- .self$searchMzRange(10, 1000, ms.mode = ms.mode, max.results = max.results)

	# Get entries
	entries <- .self$getBiodb()$getFactory()$getEntry(BIODB.MASSBANK, spectra.ids)

	# Get peaks
	df <- .self$getBiodb()$entriesToDataframe(entries, only.atomic = FALSE)
	if (BIODB.PEAK.MZ %in% colnames(df))
		mz <- df[[BIODB.PEAK.MZ]]

	# Cut
	if ( ! is.na(max.results) && length(mz) > max.results)
		mz <- mz[1:max.results]

	return(mz)
})

# Do search M/Z with tolerance {{{1
################################################################

MassbankConn$methods( .doSearchMzTol = function(mz, tol, tol.unit, min.rel.int, ms.mode, max.results) {

	# Set tolerance
	if (tol.unit == BIODB.MZTOLUNIT.PPM)
		tol <- tol * mz * 1e-6

	# Set URL
	url <- paste0(.self$getBaseUrl(), 'searchPeak?mzs=', mz)
	url <- paste0(url, '&relativeIntensity=', if (is.na(min.rel.int)) 0 else min.rel.int)
	url <- paste0(url, '&tolerance=', tol, '&instrumentTypes=all') # Tolerance seems to be the plain tolerance (i.e.: mz ± tol).
	url <- paste0(url, '&ionMode=', if (is.na(ms.mode)) 'Both' else ( if (ms.mode == BIODB.MSMODE.NEG) 'Negative' else 'Positive'))
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

# Do search M/Z range {{{1
################################################################

MassbankConn$methods( .doSearchMzRange = function(mz.min, mz.max, min.rel.int, ms.mode, max.results) {
	mz <- (mz.min + mz.max) / 2
	tol <- mz.max - mz
	return(.self$searchMzTol(mz = mz, tol = tol, tol.unit = BIODB.MZTOLUNIT.PLAIN, min.rel.int = min.rel.int, ms.mode = ms.mode, max.results = max.results))
})

# Get entry ids {{{1
################################################################

MassbankConn$methods( getEntryIds = function(max.results = NA_integer_) {

	.self$message(MSG.INFO, paste("Getting", if (is.na(max.results)) 'all' else max.results, "massbank entry ids..."))

	return(.self$searchMzTol(mz = 1000, tol = 1000, tol.unit = BIODB.MZTOLUNIT.PLAIN, min.rel.int = 100, max.results = max.results))
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
