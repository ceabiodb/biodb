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

	callSuper(...)
	.self$.abstract.class('MassbankConn')

	.self$.setDownloadExt('svn')
})

# Do get mz values {{{1
################################################################

MassbankConn$methods( .doGetMzValues = function(ms.mode, max.results, precursor, ms.level) {

	mz <- numeric(0)

	if ( ! is.null(ms.mode) && ! is.na(ms.mode))
		.self$message('debug', paste("ms.mode", ms.mode, sep = ' = '))
	if ( ! is.null(max.results) && ! is.na(max.results))
		.self$message('debug', paste("max.results", max.results, sep = ' = '))
	if ( ! is.null(precursor) && ! is.na(precursor))
		.self$message('debug', paste("precursor", precursor, sep = ' = '))
	if ( ! is.null(ms.level) && ! is.na(ms.level))
		.self$message('debug', paste("ms.level", ms.level, sep = ' = '))

	# Download
	.self$download()

	# Get list of spectra
	spectra.ids <- .self$getEntryIds()
	.self$message('debug', paste(length(spectra.ids), "spectra to process."))

	# Loop in all spectra
	i <- 0
	for (id in spectra.ids) {

		i <- i + 1
		.self$message('debug', paste("Processing entry", i, "/", length(spectra.ids), "..."))

		# Get entry
		entry <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), id)

		# Filter on mode
		if ( ! is.null(ms.mode) && ! is.na(ms.mode) && entry$getFieldValue('msmode') != ms.mode) {
			.self$message('debug', paste("Reject entry", id, "because MS mode is", entry$getFieldValue('msmode')))
			next
		}

		# Filter on ms.level
		if ( ! is.null(ms.level) && ! is.na(ms.level) && ms.level > 0 && entry$getFieldValue('ms.level') != ms.level) {
			.self$message('debug', paste("Reject entry", id, "because MS level is", entry$getFieldValue('ms.level')))
			next
		}

		# Take mz values
		new.mz <- NULL
		peaks <- entry$getFieldValue('peaks')
		if ( ! is.null(peaks) && nrow(peaks) > 0 && 'peak.mz' %in% colnames(peaks)) {
			new.mz <- peaks$peak.mz
			if (precursor && entry$hasField('msprecmz')) {
				prec.mz <- entry$getFieldValue('msprecmz', last = TRUE)
				new.mz <- if (prec.mz %in% new.mz) prec.mz else NULL
			}
		}

		# Add new M/Z values
		if ( ! is.null(new.mz)) {
			new.mz <- new.mz[ ! new.mz %in% mz]
			if (length(new.mz) > 0) {
				.self$message('debug', paste("Add", length(new.mz), "new M/Z values."))
				mz <- c(mz, new.mz)
			}
		}

		.self$message('debug', paste(length(mz), "M/Z values have been found."))

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

MassbankConn$methods( .doSearchMzTol = function(mz, mz.tol, mz.tol.unit, min.rel.int, ms.mode, max.results, precursor, ms.level) {

	returned.ids <- character()

	# Multiple M/Z values and PPM tolerance
	if (length(mz) > 1 && mz.tol.unit == 'ppm') {
		for (mz.single in mz)
			returned.ids <- c(returned.ids, .self$.doSearchMzTol(mz = mz.single, mz.tol = mz.tol, mz.tol.unit = mz.tol.unit, min.rel.int = min.rel.int, ms.mode = ms.mode, max.results = max.results, precursor = precursor, ms.level = ms.level))
		returned.ids <- returned.ids[ ! duplicated(returned.ids)]
	}

	# Single M/Z value or PLAIN tolerance
	else {

		# Set tolerance
		if (mz.tol.unit == BIODB.MZTOLUNIT.PPM)
			mz.tol <- mz.tol * mz * 1e-6

		# Build request
		max <- max.results
		if ( ! is.na(max) && (precursor || ms.level > 0))
			max <- max(10000, 10 * max)
		xml.request <- paste('<?xml version="1.0" encoding="UTF-8"?><SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:tns="http://api.massbank"><SOAP-ENV:Body><tns:searchPeak><tns:mzs>', paste(mz, collapse = ','), '</tns:mzs><tns:relativeIntensity>', if (is.na(min.rel.int)) 0 else (min.rel.int * 999 %/% 100), '</tns:relativeIntensity><tns:tolerance>', mz.tol, '</tns:tolerance><tns:instrumentTypes>all</tns:instrumentTypes><tns:ionMode>', if (is.na(ms.mode)) 'Both' else ( if (ms.mode == BIODB.MSMODE.NEG) 'Negative' else 'Positive'),'</tns:ionMode><tns:maxNumResults>', if (is.na(max)) 0 else max, '</tns:maxNumResults></tns:searchPeak></SOAP-ENV:Body></SOAP-ENV:Envelope>', sep = '')

		# Send request
		.self$message('debug', paste('Searching for M/Z values, with request: "', xml.request, '".', sep = ''))
		xmlstr <- .self$.scheduler$sendSoapRequest(paste0(.self$getBaseUrl(), 'api/services/MassBankAPI.MassBankAPIHttpSoap11Endpoint/'), xml.request)

		# Parse XML and get text
		if ( ! is.na(xmlstr)) {
			.self$message('debug', 'Parsing XML response to get IDs.')
			xml <-  XML::xmlInternalTreeParse(xmlstr, asText = TRUE)
			ns <- c(ax21 = "http://api.massbank/xsd")
			returned.ids <- XML::xpathSApply(xml, "//ax21:id", XML::xmlValue, namespaces = ns)
			.self$message('debug', paste('Found spectra ', paste(returned.ids, collapse = ', '), '.', sep = ''))

			if (ms.level > 0 || precursor) {

				# Get entries
				.self$message('debug', 'Get entries')
				entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), returned.ids, drop = FALSE)

				# Filter on precursor
				if (precursor) {
					.self$message('debug', paste('Filtering on precurssor ', precursor, '.', sep = ''))
					precursor.mz <- vapply(entries, function(x) if (is.null(x)) NA_real_ else x$getFieldValue('msprecmz', last = TRUE), FUN.VALUE = 1.0)
					precursor.matched <- (! is.na(precursor.mz)) & (precursor.mz >= mz - mz.tol) & (precursor.mz <= mz + mz.tol)
					entries <- entries[precursor.matched]
					.self$message('debug', paste(length(entries), 'entrie(s) left.'))
				}

				# Filter on ms.level
				if (ms.level > 0) {
					.self$message('debug', paste('Filtering on MS level ', ms.level, '.', sep = ''))
					ms.level.matched <- vapply(entries, function(x) if (is.null(x)) FALSE else x$getFieldValue('MS.LEVEL') == ms.level, FUN.VALUE = TRUE)
					entries <- entries[ms.level.matched]
					.self$message('debug', paste(length(entries), 'entrie(s) left.'))
				}

				.self$message('debug', 'Getting list of IDs.')
				returned.ids <- vapply(entries, function(x) x$getFieldValue('ACCESSION'), FUN.VALUE = '')
				.self$message('debug', paste('Remaining spectra are ', paste(returned.ids, collapse = ', '), '.', sep = ''))
			}
		}
	}

	# Cut
	if ( ! is.na(max.results) && length(returned.ids) > max.results) {
		.self$message('debug', 'Cut list of IDs to return.')
		returned.ids <- returned.ids[1:max.results]
	}

	return(returned.ids)
})

# Do search M/Z range {{{1
################################################################

MassbankConn$methods( .doSearchMzRange = function(mz.min, mz.max, min.rel.int, ms.mode, max.results, precursor, ms.level) {
	mz <- (mz.min + mz.max) / 2
	mz.tol <- mz.max - mz
	return(.self$searchMzTol(mz = mz, mz.tol = mz.tol, mz.tol.unit = BIODB.MZTOLUNIT.PLAIN, min.rel.int = min.rel.int, ms.mode = ms.mode, max.results = max.results, precursor = precursor, ms.level = ms.level))
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
	svn.address <- 'http://www.massbank.jp/SVN/OpenData/record/'
	.self$message('info', paste0("Download whole MassBank database from SVN server ", svn.address, "."))
	svn.cmd <- .self$getBiodb()$getConfig()$get('svn.binary.path')
	if (file.exists(svn.cmd)) {

		# Create temporary files for SVN output
		file.stdout <- tempfile('biodb.svn.stdout')
		file.stderr <- tempfile('biodb.svn.stderr')

		ret <- system2(svn.cmd, c('export', svn.address, .self$getDownloadPath()), stdout = file.stdout, stderr = file.stderr)

		# An error occured
		if (ret != 0) {
			stdout.lines <- readLines(file.stdout)
			stderr.lines <- readLines(file.stderr)
			.self$message('warning', paste0("SVN was not able to retrieve repository at ", svn.address, ". Standard output was: ", paste(stdout.lines, collapse = "."), ". Standard error was: ", paste(stderr.lines, collapse = ". "), "."))
		}

		# Remove temporary files
		unlink(c(file.stdout, file.stderr))
	}
	else
		.self$message('warning', "SVN does not seem to be installed on your system. As a consequence, Biodb is not able to download whole Massbank database. Please install SVN and make sure it is accessible in the PATH or set the environment variable BIODB_SVN_BINARY_PATH to the path of the SVN executable. If you are under Windows you may try SlikSVN at https://sliksvn.com/download/.")
})

# Do extract download {{{1
################################################################

MassbankConn$methods( .doExtractDownload = function() {

	# Copy all exported files
	.self$message('info', "Copy all MassBank record files from SVN local export directory into cache.")
	svn.files <- Sys.glob(file.path(.self$getDownloadPath(), '*', '*.txt'))
	.self$message('info', paste("Found ", length(svn.files), " record files in MassBank SVN local export directory."))
	ids <- sub('^.*/([^/]*)\\.txt$', '\\1', svn.files)
	dup.ids <- duplicated(ids)
	if (any(dup.ids))
		.self$message('caution', paste("Found duplicated IDs in downloaded Massbank records: ", paste(ids[dup.ids], collapse = ', '), '.', sep = ''))
	cache.files <- .self$getBiodb()$getCache()$getFilePath(dbid = .self$getId(), subfolder = 'shortterm', name = ids, ext = .self$getEntryContentType())
	.self$getBiodb()$getCache()$deleteFiles(dbid = .self$getId(), subfolder = 'shortterm', ext = .self$getEntryContentType())
	file.copy(svn.files, cache.files)
})

# Get entry content {{{1
################################################################

MassbankConn$methods( getEntryContent = function(entry.id) {

	# NOTE Method unused, since database is downloaded.

	# Debug
	.self$message('debug', paste0("Get entry content(s) for ", length(entry.id)," entry.id(s)..."))

	# Initialize return values
	content <- rep(NA_character_, length(entry.id))

	# Build request
	xml.request <- paste0('<?xml version="1.0" encoding="UTF-8"?><SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:tns="http://api.massbank"><SOAP-ENV:Body><tns:getRecordInfo>', paste(paste('<tns:entry.ids>', entry.id, '</tns:entry.ids>', sep = ''), collapse = ''), '</tns:getRecordInfo></SOAP-ENV:Body></SOAP-ENV:Envelope>')

	# Send request
	xmlstr <- .self$.scheduler$sendSoapRequest(paste0(.self$getBaseUrl(), 'api/services/MassBankAPI.MassBankAPIHttpSoap11Endpoint/'), xml.request)

	# Parse XML and get text
	if ( ! is.na(xmlstr)) {
		xml <-  XML::xmlInternalTreeParse(xmlstr, asText = TRUE)
		ns <- c(ax21 = "http://api.massbank/xsd")
		returned.ids <- XML::xpathSApply(xml, "//ax21:id", XML::xmlValue, namespaces = ns)
		if (length(returned.ids) > 0)
			content[match(returned.ids, entry.id)] <- XML::xpathSApply(xml, "//ax21:info", XML::xmlValue, namespaces = ns)
	}

	return(content)
})

# Get entry ids {{{1
################################################################

MassbankConn$methods( getEntryIds = function(max.results = NA_integer_, ms.level = 0) {

	# Download
	.self$download()

	# Get IDs from cache
	ids <- .self$getBiodb()$getCache()$listFiles(dbid = .self$getId(), subfolder = 'shortterm', ext = .self$getEntryContentType(), extract.name = TRUE)

	# Filter on MS level
	if ( ! is.na(ms.level) && ms.level > 0) {
		new.ids <- character(0)
		for (id in ids) {
 			entry <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), id)
			if (entry$getFieldValue('ms.level') == ms.level)
				new.ids <- c(new.ids, id)
			if ( ! is.na(max.results) && length(new.ids) >= max.results)
				break
		}
		ids <- new.ids
	}

	# Cut
	if ( ! is.na(max.results) && max.results > 0 && max.results < length(ids))
		ids <- ids[1:max.results]

	return(ids)
})

# Get nb entries {{{1
################################################################

MassbankConn$methods( getNbEntries = function(count = FALSE) {
	return(length(.self$getEntryIds()))
})

# Get chromatographic columns {{{1
################################################################

MassbankConn$methods( getChromCol = function(ids = NULL) {

	if (is.null(ids))
		ids <- .self$getEntryIds()

	# Get entries
	entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), ids)

	# Get data frame
	cols <- .self$getBiodb()$entriesToDataframe(entries, fields = c('chrom.col.id', 'chrom.col.name'))
	if (is.null(cols))
		cols <- data.frame(chrom.col.id = character(), chrom.col.name = character())

	# Remove NA values
	cols <- cols[ ! is.na(cols$chrom.col.name), , drop = FALSE]

	# Set IDs
	if ('chrom.col.id' %in% names(cols)) {
		no.ids <- is.na(cols$chrom.col.id)
		if (any(no.ids))
			cols[no.ids, 'chrom.col.id'] <- cols[no.ids, 'chrom.col.name']
	}
	else
		cols[['chrom.col.id']] <- cols$chrom.col.name

	# Remove duplicates
	cols <- cols[ ! duplicated(cols), ]

	# Rename columns
	names(cols) <- c('id', 'title')

	return(cols)
})

# Get entry page url {{{1
################################################################

MassbankConn$methods( getEntryPageUrl = function(id) {
	return(paste0(.self$getBaseUrl(), 'jsp/FwdRecord.jsp?id=', id))
})

# Get entry image url {{{1
################################################################

MassbankConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})
