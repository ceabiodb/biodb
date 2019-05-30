# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include PeakforestConn.R
#' @include BiodbMassdbConn.R
PeakforestMassConn <- methods::setRefClass("PeakforestMassConn", contains = c("PeakforestConn", "BiodbMassdbConn"))

# Constructor {{{1
################################################################

PeakforestMassConn$methods( initialize = function(...) {

	callSuper(db.name = 'spectra/lcms', ...)
})

# Get entry page url {{{1
################################################################

PeakforestMassConn$methods( getEntryPageUrl = function(id) {
	return(vapply(id, function(x) BiodbUrl(url = .self$getPropValSlot('urls', 'base.url'), params = list(PFs = x))$toString(), FUN.VALUE = ''))
})

# Get entry image url {{{1
################################################################

PeakforestMassConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})

# Peaks get range {{{2
################################################################

PeakforestMassConn$methods( ws.peaks.get.range = function(type = c('lcms', 'lcmsms'), mz.min, mz.max, mode = NA_character_, retfmt = c('plain', 'request', 'parsed', 'ids')) {

	type = match.arg(type)
	retfmt = match.arg(retfmt)

	results = character()

	if ( ! is.null(mz.min) && ! is.null(mz.max) && ! is.na(mz.min) && ! is.na(mz.max)) {

		# Build request
		params = c(token = .self$getPropertyValue('token'))
		if ( ! is.na(mode))
			params = c(params, mode = mode)
		url = BiodbUrl(url = c(.self$getPropValSlot('urls', 'ws.url'), "spectra", type, "peaks", "get-range", mz.min, mz.max), params = params)
		request = BiodbRequest(method = 'get', url = url)
		if (retfmt == 'request')
			return(request)

		# Send request
		results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

		# Parse
		results <- .self$.parseResults(results, retfmt = retfmt)
	}

	return(results)
})

# Webservice lcmsms/from-precursor
################################################################

PeakforestMassConn$methods( ws.lcmsms.from.precursor = function(prec.mz, precursorMassDelta, mode = NA_character_, retfmt = c('plain', 'request', 'parsed', 'ids')) {

	retfmt = match.arg(retfmt)

	results <- character()

	if ( ! is.null(prec.mz) && ! is.na(prec.mz)) {

		# Build request
		params <- c(token = .self$getPropertyValue('token'), precursorMassDelta = precursorMassDelta)
		if ( ! is.na(mode))
			params <- c(params, mode = mode)
		url <- BiodbUrl(url = c(.self$getPropValSlot('urls', 'ws.url'), 'spectra', 'lcmsms', 'from-precursor', prec.mz), params = params)
		request = BiodbRequest(method = 'get', url = url)
		if (retfmt == 'request')
			return(request)

		# Send request
		results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

		# Parse
		results <- .self$.parseResults(results, retfmt = retfmt)
	}

	return(results)
})

# Web service list-code-columns {{{1
################################################################

PeakforestMassConn$methods( ws.list.code.columns = function(retfmt = c('plain', 'request', 'parsed', 'data.frame')) {

	retfmt = match.arg(retfmt)

	# Build request
	url <- BiodbUrl(url = c(.self$getPropValSlot('urls', 'ws.url'), 'metadata', 'lc', 'list-code-columns'), params = list(token = .self$getPropertyValue('token')))
	request = BiodbRequest(method = 'get', url = url)
	if (retfmt == 'request')
		return(request)

	# Send request
	results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

	# Parse
	if (retfmt != 'plain') {

		# Parse results
		results <- jsonlite::fromJSON(results, simplifyDataFrame = FALSE)

		# Build data frame
		if (retfmt == 'data.frame') {
			cols <- data.frame(id = character(), title = character())
			for(id in names(results)) {
				col <- results[[id]]

				# Make title
				title <- ""
				col.fields = list(constructor = '', name = '', length = 'L', diameter = 'diam', flow_rate = 'FR', particule_size = 'PS')
				for (field in names(col.fields))
					if (field %in% names(col))
						title <- if (nchar(title) == 0) paste(col.fields[[field]], col[[field]]) else paste(title, col.fields[[field]], col[[field]])

				# Add col to data frame
				cols <- rbind(cols, data.frame(id = id, title = title, stringsAsFactors = FALSE))
			}
			.self$message('debug', paste('Found', nrow(cols), 'chromatographic columns.'))
			results = cols
		}
	}

	return(results)
})

# Get chromatographic columns {{{1
################################################################

PeakforestMassConn$methods( getChromCol = function(ids = NULL) {

	cols <- .self$ws.list.code.columns(retfmt = 'data.frame')

	# Restrict to set of spectra
	if ( ! is.null(ids)) {
		entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), ids)
		selected.cols <- .self$getBiodb()$entriesToDataframe(entries, fields = 'chrom.col.id', drop = TRUE)

		# Filter cols data frame
		cols <- cols[cols$id %in% selected.cols, ]

		.self$message('debug', paste('Restricted set of chromatographic columns to', nrow(cols), 'after filtering on spectra IDs.'))
	}

	return(cols)
})

# Private methods {{{1
################################################################

# Get mz values {{{2
################################################################

PeakforestMassConn$methods( .doGetMzValues = function(ms.mode, max.results, precursor, ms.level) {

	mz <- NULL
	if (ms.level > 0 || precursor) {

		# Get all IDs
		ids <- .self$getEntryIds()

		# Loop on all IDs
		for (id in ids) {

			entry <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), id)
			.self$.assert.not.null(entry)

			if (ms.level > 0 && ( ! entry$hasField('ms.level') || entry$getFieldValue('ms.level') != ms.level))
				next

			if (precursor) {
				if (entry$hasField('msprecmz'))
					mz <- c(mz, entry$getFieldValue('msprecmz'))
			}
			else {
				if (entry$hasField('peaks'))
					mz <- c(mz, entry$getFieldValue('peaks')[['peak.mz']])
			}

			if ( ! is.na(max.results) && length(mz) > max.results)
				break
		}
	}

	else {
		# Set URL
		url <- paste(.self$getPropValSlot('urls', 'ws.url'), 'spectra/lcms/peaks/list-mz?token=', .self$getPropertyValue('token'), sep = '')
		if ( ! is.na(ms.mode))
			url <- paste(url, '&mode=', if (ms.mode == 'pos') 'positive' else 'negative', sep ='')

		# Get MZ values
		json.str <- .self$getBiodb()$getRequestScheduler()$getUrl(url)
		.self$.checkIfError(json.str)

		# Parse JSON
		mz <- jsonlite::fromJSON(json.str, simplifyDataFrame = FALSE)
	}

	# Apply cut-off
	if ( ! is.na(max.results) && length(mz) > max.results)
		mz <- mz[seq_len(max.results)]

	return(mz)
})

# Parse IDs from JSON {{{2
################################################################

PeakforestMassConn$methods( .parseResults = function(results, retfmt) {

	if (retfmt != 'plain') {

		# Parse
		results <- jsonlite::fromJSON(results, simplifyDataFrame = FALSE)

		if(retfmt == 'ids') {

			if (length(results) == 0)
				results = character()

			# Extract IDs
			else {
				ids <- vapply(results, function(x) if ('id' %in% names(x)) x$id else (if ('source' %in% names(x) && ! is.null(x$source) && 'id' %in% names(x$source)) x$source$id else NA_integer_), FUN.VALUE = 1)
				ids <- ids[ ! is.na(ids)] # Remove NA values (some returned matches have no source information).
				ids <- unlist(ids)
				results <- as.character(ids)
			}
		}
	}

	return(results)
})



# Do search M/Z range {{{2
################################################################

PeakforestMassConn$methods( .doSearchMzRange = function(mz.min, mz.max, min.rel.int, ms.mode, max.results, precursor, ms.level) {

	ids <- character()

	# Multiple M/Z ranges
	if (length(mz.min) > 1) {
		for (i in seq_along(mz.min))
			ids <- c(ids, .self$.doSearchMzRange(mz.min = mz.min[[i]], mz.max = mz.max[[i]], min.rel.int = min.rel.int, ms.mode = ms.mode, max.results = max.results, precursor = precursor, ms.level = ms.level))
		ids <- ids[ ! duplicated(ids)]
	}
	
	# Single M/Z range
	else {
		mode <- if ( ! is.na(ms.mode)) (if (ms.mode == 'neg') 'NEG' else 'POS') else NA_character_
		if (ms.level == 0 || ms.level == 1)
			ids <- .self$ws.peaks.get.range('lcms', mz.min, mz.max, mode = mode, retfmt = 'ids')
		if (ms.level == 0 || ms.level == 2) {
			if (precursor)
				ids <- c(ids, .self$ws.lcmsms.from.precursor((mz.min + mz.max) / 2, (mz.max - mz.min) / 2, mode = mode, retfmt = 'ids'))
			else
				ids <- c(ids, .self$ws.peaks.get.range('lcmsms', mz.min, mz.max, mode = mode, retfmt = 'ids'))
		}
	}

	# Filtering
	if ( length(ids) > 0 && ( length(mz.min) > 1 || ! is.na(min.rel.int) || precursor)) {

		entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), ids, drop = FALSE)

		# Filter on precursor
		if (precursor && (ms.level %in% c(0, 1) || length(mz.min > 1)))
			entries <- lapply(entries, function(e) if ( ! is.null(e) && e$hasField('msprecmz') && any(e$getFieldValue('msprecmz') <= mz.max & e$getFieldValue('msprecmz') >= mz.min)) e else NULL)

		# Filter on M/Z ranges when there are more than one range, and also on intensity
		if (length(mz.min) > 1 || ! is.na(min.rel.int)) {
			filtered.entries <- NULL
			for (e in entries) {

				good.entry <- FALSE
				if (! is.null(e) && e$hasField('peaks')) {
					peaks <- e$getFieldValue('peaks')
					good.entry <- TRUE
					for (i in seq_along(mz.min)) {
						in.range <- (peaks[['peak.mz']] >= mz.min[[i]] & peaks[['peak.mz']] <= mz.max[[i]])
						if ( ! any(in.range) || ( ! is.na(min.rel.int) && ! any(peaks[in.range, 'peak.relative.intensity'] >= min.rel.int))) {
							good.entry <- FALSE
							break
						}
					}
				}

				# Append entry to list
				filtered.entries <- c(filtered.entries, if (good.entry) e else list(NULL))
			}
			entries <- filtered.entries
		}

		# Select entries
		ids <- ids[ ! vapply(entries, is.null, FUN.VALUE = TRUE)]
	}

	# Cut
	if ( ! is.na(max.results) && length(ids) > max.results)
		ids <- ids[seq_len(max.results)]

	return(ids)
})

# Do get entry content request {{{2
################################################################

PeakforestMassConn$methods( .doGetEntryContentRequest = function(id, concatenate = TRUE) {

	# Check token
	if (is.na(.self$getPropertyValue('token')))
		.self$message('error', "Peakforest requires a token for this service.")

	# IDs are unique between LCMS and LCMSMS. Hence no confusion possible, and ID used in LCMS is not used in LCMSMS.
	if (concatenate) {
		url <- paste(.self$getPropValSlot('urls', 'ws.url'), 'spectra/lcms/ids/', paste(id, collapse = ','),'?token=', .self$getPropertyValue('token'), sep = '')
		url <- c(url, paste(.self$getPropValSlot('urls', 'ws.url'), 'spectra/lcmsms/ids/', paste(id, collapse = ','),'?token=', .self$getPropertyValue('token'), sep = ''))
	}
	else {
		url <- paste(.self$getPropValSlot('urls', 'ws.url'), 'spectra/lcms/ids/', id,'?token=', .self$getPropertyValue('token'), sep = '')
		url <- c(url, paste(.self$getPropValSlot('urls', 'ws.url'), 'spectra/lcmsms/ids/', id,'?token=', .self$getPropertyValue('token'), sep = ''))
	}

	return(url)
})

