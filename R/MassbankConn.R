# vi: fdm=marker

#' @include BiodbRemotedbConn.R
#' @include BiodbMassdbConn.R
#' @include BiodbDownloadable.R

# Constants {{{1
################################################################

# Parsing expressions {{{2
################################################################

.BIODB.MASSBANK.PARSING.EXPR <- list(
	'accession'             = "^ACCESSION: (.+)$",
	'name'                  = "^CH\\$NAME:\\s+(.+)$",
	'msdev'                 = "^AC\\$INSTRUMENT: (.+)$",
	'msdevtype'             = "^AC\\$INSTRUMENT_TYPE: (.+)$",
	'mstype'                = "^AC\\$MASS_SPECTROMETRY: MS_TYPE (.+)$",
	'nb.peaks'              = "^PK\\$NUM_PEAK: ([0-9]+)$",
	'msprecannot'           = "^MS\\$FOCUSED_ION: PRECURSOR_TYPE (.+)$",
	'inchi'                 = "^CH\\$IUPAC:\\s+(.+)$",
	'inchikey'              = "^CH\\$LINK: INCHIKEY\\s+(.+)$",
	'chemspider.id'         = "^CH\\$LINK: CHEMSPIDER\\s+(.+)$",
	'chebi.id'              = "^CH\\$LINK: CHEBI\\s+(.+)$",
	'kegg.compound.id'      = "^CH\\$LINK: KEGG\\s+(.+)$",
	'cas.id'                = "^CH\\$LINK: CAS\\s+(.+)$",
	'ncbi.pubchem.comp.id'  = "^CH\\$LINK: PUBCHEM\\s+((CID:)?[0-9]+)",
	'ncbi.pubchem.subst.id' = "^CH\\$LINK: PUBCHEM\\s+.*(SID:[0-9]+)",
	'hmdb.metabolites.id'   = "^CH\\$LINK: HMDB\\s+(HMDB[0-9]+)",
	'formula'               = "^CH\\$FORMULA:\\s+(.+)$",
	'smiles'                = "^CH\\$SMILES:\\s+(.+)$",
	'exact.mass'            = "^CH\\$EXACT_MASS:\\s+(.+)$",
	'msmode'                = "^AC\\$MASS_SPECTROMETRY: ION_MODE (.+)$",
	'chrom.col.name'        = "^AC\\$CHROMATOGRAPHY: COLUMN_NAME\\s+(.+)$",
	'chrom.solvent'         = "^AC\\$CHROMATOGRAPHY: SOLVENT\\s+(.+)$",
	'chrom.flow.rate'       = "^AC\\$CHROMATOGRAPHY: FLOW_RATE\\s+(.+)$",
	'chrom.flow.gradient'   = "^AC\\$CHROMATOGRAPHY: FLOW_GRADIENT\\s+(.+)$"
)

# Class declaration {{{1
################################################################

MassbankConn <- methods::setRefClass("MassbankConn", contains = c("BiodbRemotedbConn", "BiodbMassdbConn", 'BiodbDownloadable'), fields = list(.prefix2dns = 'list'))

# Constructor {{{1
################################################################0

MassbankConn$methods( initialize = function(...) {

	callSuper(...)

	.prefix2dns <<- list()
	.self$.setDownloadExt('tar.gz')
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

# Requires download {{{1
################################################################

MassbankConn$methods( requiresDownload = function() {
	return(TRUE)
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
	xmlstr <- .self$getBiodb()$getRequestScheduler()$sendSoapRequest(paste0(.self$getUrl('base.url'), 'api/services/MassBankAPI.MassBankAPIHttpSoap11Endpoint/'), xml.request)

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

	# Remove duplicates
	cols <- cols[ ! duplicated(cols), ]

	# Rename columns
	names(cols) <- c('id', 'title')

	return(cols)
})

# Get dns from id {{{1
################################################################

MassbankConn$methods( getDns = function(id) {

	# Download prefixes file, parse it and build list
	if (length(.self$.prefix2dns) == 0)
		.self$.loadPrefixes()

	dns <- vapply(id, function(x) {
		prefix <- sub('^([A-Z]+)[0-9]+$', '\\1', x, perl = TRUE)
		if (prefix %in% names(.self$.prefix2dns))
			.self$.prefix2dns[[prefix]]
		else
			NA_character_
		}, FUN.VALUE = '', USE.NAMES = FALSE)

	return(dns)
})

# Get entry page url {{{1
################################################################

MassbankConn$methods( getEntryPageUrl = function(id) {
	return(vapply(id, function(x) BiodbUrl(url = c(.self$getUrl('base.url'), 'MassBank', 'RecordDisplay.jsp'), params = list(id = x, dsn = .self$getDns(x)))$toString(), FUN.VALUE = ''))
})

# Get entry image url {{{1
################################################################

MassbankConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})

# Private methods {{{1
################################################################

# Get parsing expressions {{{2
################################################################

MassbankConn$methods( .getParsingExpressions = function() {
	return(.BIODB.MASSBANK.PARSING.EXPR)
})

# Do download {{{2
################################################################

MassbankConn$methods( .doDownload = function() {

	# Download tar.gz
	tar.gz.url <- BiodbUrl(url = .self$getUrl('db.tar.url'))
	.self$message('info', paste0("Downloading \"", tar.gz.url$toString(), "\"..."))
	scheduler <- .self$getBiodb()$getRequestScheduler()
	path <- .self$getDownloadPath()
	scheduler$downloadFile(url = tar.gz.url, dest.file = path)
})

# Do extract download {{{2
################################################################

MassbankConn$methods( .doExtractDownload = function() {

	# Extract
	extracted.dir <- tempfile(.self$getId())
	untar(tarfile = .self$getDownloadPath(), exdir = extracted.dir) 

	# Copy all exported files
	.self$message('info', "Copy all extracted MassBank record files into cache.")
	record.files <- Sys.glob(file.path(extracted.dir, 'MassBank-data-master', '*', '*.txt'))
	.self$message('info', paste("Found ", length(record.files), " record files in MassBank GitHub archive."))
	ids <- sub('^.*/([^/]*)\\.txt$', '\\1', record.files)
	dup.ids <- duplicated(ids)
	if (any(dup.ids))
		.self$message('caution', paste("Found duplicated IDs in downloaded Massbank records: ", paste(ids[dup.ids], collapse = ', '), '.', sep = ''))
	cache.files <- .self$getBiodb()$getCache()$getFilePath(.self$getCacheId(), subfolder = 'shortterm', name = ids, ext = .self$getEntryContentType())
	.self$getBiodb()$getCache()$deleteFiles(.self$getCacheId(), subfolder = 'shortterm', ext = .self$getEntryContentType())
	file.copy(record.files, cache.files)

	# Delete extracted dir
	unlink(extracted.dir, recursive = TRUE)
})

# Do search M/Z range {{{2
################################################################

MassbankConn$methods( .doSearchMzRange = function(mz.min, mz.max, min.rel.int, ms.mode, max.results, precursor, ms.level) {
	mz <- (mz.min + mz.max) / 2
	mz.tol <- mz.max - mz
	return(.self$searchMzTol(mz = mz, mz.tol = mz.tol, mz.tol.unit = 'plain', min.rel.int = min.rel.int, ms.mode = ms.mode, max.results = max.results, precursor = precursor, ms.level = ms.level))
})

# Do search M/Z with tolerance {{{2
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
		if (mz.tol.unit == 'ppm')
			mz.tol <- mz.tol * mz * 1e-6

		# Build request
		max <- max.results
		if ( ! is.na(max) && (precursor || ms.level > 0))
			max <- max(10000, 10 * max)
		xml.request <- paste('<?xml version="1.0" encoding="UTF-8"?><SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:tns="http://api.massbank"><SOAP-ENV:Body><tns:searchPeak><tns:mzs>', paste(mz, collapse = ','), '</tns:mzs><tns:relativeIntensity>', if (is.na(min.rel.int)) 0 else (min.rel.int * 999 %/% 100), '</tns:relativeIntensity><tns:tolerance>', mz.tol, '</tns:tolerance><tns:instrumentTypes>all</tns:instrumentTypes><tns:ionMode>', if (is.na(ms.mode)) 'Both' else ( if (ms.mode == 'neg') 'Negative' else 'Positive'),'</tns:ionMode><tns:maxNumResults>', if (is.na(max)) 0 else max, '</tns:maxNumResults></tns:searchPeak></SOAP-ENV:Body></SOAP-ENV:Envelope>', sep = '')

		# Send request
		.self$message('debug', paste('Searching for M/Z values, with request: "', xml.request, '".', sep = ''))
		xmlstr <- .self$getBiodb()$getRequestScheduler()$sendSoapRequest(paste0(.self$getUrl('base.url'), 'api/services/MassBankAPI.MassBankAPIHttpSoap11Endpoint/'), xml.request)

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

# Load prefixes {{{2
################################################################

MassbankConn$methods( .loadPrefixes = function() {


	# Get prefixes file content
	prefixes.filepath <- .self$getBiodb()$getCache()$getFilePath(.self$getCacheId(), subfolder = 'longterm', name = 'prefixes', ext = 'md')
	if ( ! file.exists(prefixes.filepath))
		.self$getBiodb()$getRequestScheduler()$downloadFile(url = BiodbUrl(url = .self$getUrl('prefixes.file.url')), dest.file = prefixes.filepath)

	# Split in lines
	lines <- readLines(prefixes.filepath)

	# Skip header lines
	lines <- lines[3:length(lines)]

	# Parse databases and prefixes
	results <- stringr::str_match_all(lines, '^\\| *([^ ]+) +\\|[^|]+\\|[^|]+\\| *([A-Z ,]+) *\\|.*$')
	dbs <- vapply(results, function(x) x[1,2], FUN.VALUE='')
	prefixes <- lapply(results, function(x) stringr::str_match_all(x[1,3], '([A-Z]+)')[[1]][,1])

	# Loop on all databases
	for (i in seq_along(dbs)) {

		# Loop on prefixes
		for (p in prefixes[[i]])
			.self$.prefix2dns[[p]] <- dbs[[i]]
	}
})

# Get entry ids {{{2
################################################################

MassbankConn$methods( .doGetEntryIds = function(max.results = NA_integer_, ms.level = 0) {

	# Download
	.self$download()

	# Get IDs from cache
	ids <- .self$getBiodb()$getCache()$listFiles(.self$getCacheId(), subfolder = 'shortterm', ext = .self$getEntryContentType(), extract.name = TRUE)

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

	return(ids)
})

