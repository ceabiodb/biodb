# vi: fdm=marker

#' @include RemotedbConn.R
#' @include MassdbConn.R
#' @include BiodbDownloadable.R

# Constants {{{1
################################################################

MASSBANK.JP.URL  <- 'http://www.massbank.jp/'
MASSBANK.EU.URL  <- 'http://massbank.eu/'

# Class declaration {{{1
################################################################

MassbankConn <- methods::setRefClass("MassbankConn", contains = c("RemotedbConn", "MassdbConn", "BiodbDownloadable"))

# Constructor {{{1
################################################################0

MassbankConn$methods( initialize = function(...) {

	callSuper(content.type = BIODB.TXT, base.url = c(MASSBANK.EU.URL, MASSBANK.JP.URL), ...)
})

# Send URL request {{{1
################################################################

MassbankConn$methods( .send.url.request = function(url) {

	# Find an available server
	base.url.indices <- seq(length(.self$.base.url))
	success <- FALSE
	while ( ! success && length(base.url.indices) > 0) {

		# Send request
		result <- .self$.getUrlScheduler()$getUrl(url)

		# Test if a server error occured
		if (length(grep("Object not found", result)) > 0 || length(grep("Not Found", result)) > 0 || length(grep("The service cannot be found", result)) > 0) {
			.self$message(MSG.INFO, paste("The service on Massbank website \"", .self$getBaseUrl(), "\" cannot be found.", sep = ''))

			# Remove current URL from list
			base.url.indices <- base.url.indices[.self$.base.url.index != base.url.indices] # remove current URL
			if (length(base.url.indices) > 0) {
				old.base.url <- .self$getBaseUrl()
				.base.url.index <<- base.url.indices[[1]]
				new.base.url <- .self$getBaseUrl()
				.self$message(MSG.INFO, paste("Trying \"", .self$getBaseUrl(), "\"...", sep = ''))

				# Replace base URL
				url <- sub(old.base.url, new.base.url, url)
			}
		}
		else
			success <- TRUE
	}
	if ( ! success)
		.self$message(MSG.ERROR, "No Massbank website available.")

	return(result)
})

# Get entry content {{{1
################################################################

MassbankConn$methods( getEntryContent = function(id) {

	# Debug
	.self$message(MSG.DEBUG, paste0("Get entry content(s) for ", length(id)," id(s)..."))

	# Initialize return values
	content <- rep(NA_character_, length(id))

	URL.MAX.LENGTH <- 2083

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

MassbankConn$methods( .doGetMzValues = function(ms.mode, max.results, precursor, ms.level) {

	                           # TODO Add some filtering on precursor and MS level
	mz <- numeric(0)

	# Get list of spectra
	.self$message(MSG.DEBUG, paste('max.results=', max.results, sep = ''))
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

	ids <- NULL

	# Do we allow database download? This can take some time.
	if (.self$getBiodb()$getConfig()$isEnabled(CFG.ALLOW.HUGE.DOWNLOADS)) {

		# Download
		.self$download()

		# Get IDs from cache
		ids <- .self$getBiodb()$getCache()$listFiles(db = .self$getId(), folder = CACHE.SHORT.TERM.FOLDER, ext = .self$getEntryContentType(), extract.names = TRUE)

		# Cut
		if ( ! is.na(max.results) && max.results < length(ids))
			ids <- ids[1:max.results]
	}

	return(ids)
})

# Get nb entries {{{1
################################################################

MassbankConn$methods( getNbEntries = function(count = FALSE) {
	return(length(.self$getEntryIds()))
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

# Do download {{{1
################################################################

MassbankConn$methods( .doDownload = function() {

	.self$message(MSG.INFO, "Extract whole MassBank database.")

	# SVN export
	svn.path <- .self$getBiodb()$getCache()$getFilePaths(db = .self$getId(), folder = CACHE.LONG.TERM.FOLDER, names = 'download', ext = 'svn')
	if ( ! .self$getBiodb()$getConfig()$get(CFG.OFFLINE) && ! file.exists(svn.path)) {
		.self$message(MSG.INFO, "Download whole MassBank database from SVN server.")
		svn.cmd <- .self$getBiodb()$getConfig()$get(CFG.SVN.BINARY.PATH)
		system2(svn.cmd, c('export', '--force', '--quiet', 'http://www.massbank.jp/SVN/OpenData/record/', svn.path))
	}

	# Copy all exported files
	.self$message(MSG.INFO, "Copy all MassBank record files from SVN local export directory into cache.")
	svn.files <- Sys.glob(file.path(svn.path, '*', '*.txt'))
	.self$message(MSG.INFO, paste("Found ", length(svn.files), " record files in MassBank SVN local export directory."))
	ids <- sub('^.*/([^/]*)\\.txt$', '\\1', svn.files)
	dup.ids <- duplicated(ids)
	if (any(dup.ids))
		.self$message(MSG.CAUTION, paste("Found duplicated IDs in downloaded Massbank records: ", paste(ids[dup.ids], collapse = ', '), '.', sep = ''))
	cache.files <- .self$getBiodb()$getCache()$getFilePaths(db = .self$getId(), folder = CACHE.SHORT.TERM.FOLDER, names = ids, ext = .self$getEntryContentType())
	.self$getBiodb()$getCache()$deleteFiles(db = .self$getId(), folder = CACHE.SHORT.TERM.FOLDER, ext = .self$getEntryContentType())
	file.copy(svn.files, cache.files)

	return(TRUE)
})
