# vi: fdm=marker

#' @include RemotedbConn.R
#' @include MassdbConn.R
#' @include BiodbDownloadable.R

# Class declaration {{{1
################################################################

MassbankConn <- methods::setRefClass("MassbankConn", contains = c("RemotedbConn", "MassdbConn", 'BiodbDownloadable'))

# Constructor {{{1
################################################################0

MassbankConn$methods( initialize = function(...) {

	callSuper(content.type = BIODB.TXT, ...)

	.self$.setDownloadExt('svn')
})

# Do get mz values {{{1
################################################################

MassbankConn$methods( .doGetMzValues = function(ms.mode, max.results, precursor, ms.level) {

	mz <- numeric(0)

	if ( ! is.null(ms.mode) && ! is.na(ms.mode))
		.self$message(MSG.DEBUG, paste("ms.mode", ms.mode, sep = ' = '))
	if ( ! is.null(max.results) && ! is.na(max.results))
		.self$message(MSG.DEBUG, paste("max.results", max.results, sep = ' = '))
	if ( ! is.null(precursor) && ! is.na(precursor))
		.self$message(MSG.DEBUG, paste("precursor", precursor, sep = ' = '))
	if ( ! is.null(ms.level) && ! is.na(ms.level))
		.self$message(MSG.DEBUG, paste("ms.level", ms.level, sep = ' = '))

	# Download
	.self$download()

	# Get list of spectra
	spectra.ids <- .self$getEntryIds()

	# Loop in all spectra
	i <- 0
	for (id in spectra.ids) {

		i <- i + 1
		.self$message(MSG.DEBUG, paste("Processing entry", i, "/", length(spectra.ids), "..."))

		# Get entry
		entry <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), id)

		# Filter on mode
		if ( ! is.null(ms.mode) && ! is.na(ms.mode) && entry$getFieldValue(BIODB.MSMODE) != ms.mode) {
			.self$message(MSG.DEBUG, paste("Reject entry", id, "because MS mode is", entry$getFieldValue(BIODB.MSMODE)))
			next
		}

		# Filter on ms.level
		if ( ! is.null(ms.level) && ! is.na(ms.level) && ms.level > 0 && entry$getFieldValue(BIODB.MS.LEVEL) != ms.level) {
			.self$message(MSG.DEBUG, paste("Reject entry", id, "because MS level is", entry$getFieldValue(BIODB.MS.LEVEL)))
			next
		}

		# Take mz values
		new.mz <- NULL
		if (precursor) {
			if (entry$hasField(BIODB.MSPRECMZ))
				new.mz <- entry$getFieldValue(BIODB.MSPRECMZ, last = TRUE)
		} else {
			peaks <- entry$getFieldValue(BIODB.PEAKS)
			if ( ! is.null(peaks) && nrow(peaks) > 0 && BIODB.PEAK.MZ %in% colnames(peaks))
				new.mz <- peaks[[BIODB.PEAK.MZ]]
		}

		# Add new M/Z values
		if ( ! is.null(new.mz)) {
			new.mz <- new.mz[ ! new.mz %in% mz]
			if (length(new.mz) > 0) {
				.self$message(MSG.DEBUG, paste("Add", length(new.mz), "new M/Z values."))
				mz <- c(mz, new.mz)
			}
		}

		.self$message(MSG.DEBUG, paste(length(mz), "M/Z values have been found."))

		# Stop if max reached
		if ( ! is.null(max.results) && ! is.na(max.results) && length(mz) >= max.results)
			break
	}

	# Cut
	if ( ! is.na(max.results) && length(mz) > max.results)
		mz <- mz[1:max.results]

	return(mz)
})

# Do search M/Z with tolerance {{{1
################################################################

MassbankConn$methods( .doSearchMzTol = function(mz, tol, tol.unit, min.rel.int, ms.mode, max.results, precursor, ms.level) {

	returned.ids <- NULL

	# Set tolerance
	if (tol.unit == BIODB.MZTOLUNIT.PPM)
		tol <- tol * mz * 1e-6

	# Build request
	max <- max.results
	if ( ! is.na(max) && (precursor || ms.level > 0))
		max <- max(10000, 10 * max)
	xml.request <- paste('<?xml version="1.0" encoding="UTF-8"?><SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:tns="http://api.massbank"><SOAP-ENV:Body><tns:searchPeak><tns:mzs>', mz, '</tns:mzs><tns:relativeIntensity>', if (is.na(min.rel.int)) 0 else min.rel.int, '</tns:relativeIntensity><tns:tolerance>', tol, '</tns:tolerance><tns:instrumentTypes>all</tns:instrumentTypes><tns:ionMode>', if (is.na(ms.mode)) 'Both' else ( if (ms.mode == BIODB.MSMODE.NEG) 'Negative' else 'Positive'),'</tns:ionMode><tns:maxNumResults>', if (is.na(max)) 0 else max, '</tns:maxNumResults></tns:searchPeak></SOAP-ENV:Body></SOAP-ENV:Envelope>', sep = '')

	# Send request
	xmlstr <- .self$.scheduler$sendSoapRequest(paste0(.self$getBaseUrl(), 'api/services/MassBankAPI.MassBankAPIHttpSoap11Endpoint/'), xml.request)

	# Parse XML and get text
	if ( ! is.na(xmlstr)) {
		xml <-  XML::xmlInternalTreeParse(xmlstr, asText = TRUE)
		ns <- c(ax21 = "http://api.massbank/xsd")
		returned.ids <- XML::xpathSApply(xml, "//ax21:id", XML::xmlValue, namespaces = ns)

		if (ms.level > 0 || precursor) {

			# Get entries
			entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), returned.ids, drop = FALSE)

			# Filter on precursor
			if (precursor) {
				precursor.mz <- vapply(entries, function(x) if (is.null(x)) NA_real_ else x$getFieldValue(BIODB.MSPRECMZ, last = TRUE), FUN.VALUE = 1.0)
				precursor.matched <- ! is.na(precursor.mz) & (precursor.mz >= mz - tol) & (precursor.mz <= mz + tol)
				entries <- entries[precursor.matched]
			}

			# Filter on ms.level
			if (ms.level > 0) {
				ms.level.matched <- vapply(entries, function(x) if (is.null(x)) FALSE else x$getFieldValue(BIODB.MS.LEVEL) == ms.level, FUN.VALUE = TRUE)
				entries <- entries[ms.level.matched]
			}

			returned.ids <- vapply(entries, function(x) x$getFieldValue(BIODB.ACCESSION), FUN.VALUE = '')
		}
	}

	# Cut
	if ( ! is.na(max.results) && length(returned.ids) > max.results)
		returned.ids <- returned.ids[1:max.results]

	return(returned.ids)
})

# Do search M/Z range {{{1
################################################################

MassbankConn$methods( .doSearchMzRange = function(mz.min, mz.max, min.rel.int, ms.mode, max.results, precursor, ms.level) {
	mz <- (mz.min + mz.max) / 2
	tol <- mz.max - mz
	return(.self$searchMzTol(mz = mz, tol = tol, tol.unit = BIODB.MZTOLUNIT.PLAIN, min.rel.int = min.rel.int, ms.mode = ms.mode, max.results = max.results, precursor = precursor, ms.level = ms.level))
})

# Do get entry content url {{{1
################################################################

MassbankConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {

	                  # TODO Return an URL request object with SOAP message embedded
	if (concatenate)
		url <- paste(.self$getBaseUrl(), 'getRecordInfo?ids=', paste(id, collapse = ','), sep = '')
	else
		url <- paste(.self$getBaseUrl(), 'getRecordInfo?ids=', id, sep = '')

	return(url)
})

# Do download {{{1
################################################################

MassbankConn$methods( .doDownload = function() {

	# SVN export
	.self$message(MSG.INFO, "Download whole MassBank database from SVN server.")
	svn.cmd <- .self$getBiodb()$getConfig()$get(CFG.SVN.BINARY.PATH)
	system2(svn.cmd, c('export', '--force', '--quiet', 'http://www.massbank.jp/SVN/OpenData/record/', .self$getDownloadPath()))
})

# Do extract download {{{1
################################################################

MassbankConn$methods( .doExtractDownload = function() {

	# Copy all exported files
	.self$message(MSG.INFO, "Copy all MassBank record files from SVN local export directory into cache.")
	svn.files <- Sys.glob(file.path(.self$getDownloadPath(), '*', '*.txt'))
	.self$message(MSG.INFO, paste("Found ", length(svn.files), " record files in MassBank SVN local export directory."))
	ids <- sub('^.*/([^/]*)\\.txt$', '\\1', svn.files)
	dup.ids <- duplicated(ids)
	if (any(dup.ids))
		.self$message(MSG.CAUTION, paste("Found duplicated IDs in downloaded Massbank records: ", paste(ids[dup.ids], collapse = ', '), '.', sep = ''))
	cache.files <- .self$getBiodb()$getCache()$getFilePaths(db = .self$getId(), folder = CACHE.SHORT.TERM.FOLDER, names = ids, ext = .self$getEntryContentType())
	.self$getBiodb()$getCache()$deleteFiles(db = .self$getId(), folder = CACHE.SHORT.TERM.FOLDER, ext = .self$getEntryContentType())
	file.copy(svn.files, cache.files)
})

# Get entry content {{{1
################################################################

MassbankConn$methods( getEntryContent = function(id) {

	# NOTE Method unused, since database is downloaded.

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

MassbankConn$methods( getEntryIds = function(max.results = NA_integer_) {

	# Download
	.self$download()

	# Get IDs from cache
	ids <- .self$getBiodb()$getCache()$listFiles(db = .self$getId(), folder = CACHE.SHORT.TERM.FOLDER, ext = .self$getEntryContentType(), extract.names = TRUE)

	# Cut
	if ( ! is.na(max.results) && max.results < length(ids))
		ids <- ids[1:max.results]

	return(ids)
})

# Get nb entries {{{1
################################################################

MassbankConn$methods( getNbEntries = function(count = FALSE) {
	return(length(.self$getEntryIds()))
})
