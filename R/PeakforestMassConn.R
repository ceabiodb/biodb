# vi: fdm=marker

# Constants {{{1
################################################################

.BIODB.PEAKFOREST.MASS.PARSING.EXPR <- list(
	'accession'                 = "id",
	'msmode'                    = "polarity",
	'msdev'                     = c('analyzerMassSpectrometerDevice', 'instrumentName'),
	'msdevtype'                 = c('analyzerMassSpectrometerDevice', 'ionAnalyzerType'),
	'mstype'                    = 'type',
	'msprecmz'                  = 'parentIonMZ',
	'chrom.col.name'            = c('liquidChromatography', 'columnName'),
	'chrom.col.id'              = c('liquidChromatography', 'columnCode'),
	'chrom.col.constructor'     = c('liquidChromatography', 'columnConstructorAString'),
	'chrom.col.length'          = c('liquidChromatography', 'columnLength'),
	'chrom.col.diameter'        = c('liquidChromatography', 'columnDiameter'),
	'chrom.col.rt.min'          = 'RTmin',
	'chrom.col.rt.max'          = 'RTmax',
	'chrom.col.method.protocol' = c('liquidChromatography', 'methodProtocol')
)

# Class declaration {{{1
################################################################

#' @include PeakforestConn.R
#' @include MassdbConn.R
PeakforestMassConn <- methods::setRefClass("PeakforestMassConn", contains = c("PeakforestConn", "MassdbConn"))

# Constructor {{{1
################################################################

PeakforestMassConn$methods( initialize = function(...) {

	callSuper(db.name = 'spectra/lcms', ...)
})

# Get entry content request {{{1
################################################################

PeakforestMassConn$methods( .doGetEntryContentRequest = function(id, concatenate = TRUE) {

	# Check token
	if (is.na(.self$getToken()))
		.self$message('error', "Peakforest requires a token for this service.")

	# IDs are unique between LCMS and LCMSMS. Hence no confusion possible, and ID used in LCMS is not used in LCMSMS.
	if (concatenate) {
		url <- paste(.self$getBaseUrl(), 'spectra/lcms/ids/', paste(id, collapse = ','),'?token=', .self$getToken(), sep = '')
		url <- c(url, paste(.self$getBaseUrl(), 'spectra/lcmsms/ids/', paste(id, collapse = ','),'?token=', .self$getToken(), sep = ''))
	}
	else {
		url <- paste(.self$getBaseUrl(), 'spectra/lcms/ids/', id,'?token=', .self$getToken(), sep = '')
		url <- c(url, paste(.self$getBaseUrl(), 'spectra/lcmsms/ids/', id,'?token=', .self$getToken(), sep = ''))
	}

	return(url)
})

# Get entry page url {{{1
################################################################

PeakforestMassConn$methods( getEntryPageUrl = function(id) {
	return(paste('https://metabohub.peakforest.org/webapp/home?PFs=', id))
})

# Get entry image url {{{1
################################################################

PeakforestMassConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})

# Create reduced entry {{{1
################################################################

PeakforestMassConn$methods( createReducedEntry = function(content , drop = TRUE){
	entries <- vector(length(content), mode = "list")
	jsonfields <- character()
	
	###Checking that it's a list.
	if (length(content) == 1) {
		if (startsWith(content[[1]], "<html>")) {
			return(NULL)
		} else{
			content <- jsonlite::fromJSON(content[[1]], simplifyDataFrame=FALSE)
			
		}
	}
	if(length(content)==0){
		return(list())
	}
	
	for (i in seq_along(content)) {
		single.content <- content[[i]]
		jsontree <- NULL
		if (typeof(single.content) == "character") {
			if (startsWith(single.content, "<html>") | single.content == "null") {
				entries[[i]] <- NULL
				next
			}
			jsontree <- jsonlite::fromJSON(single.content, simplifyDataFrame=FALSE)
		} else{
			jsontree <- content
		}
		
		
		cnames <-
			c(
				'PEAK.MZ',
				'PEAK.RELATIVE.INTENSITY',
				'PEAK.FORMULA',
				'PEAK.MZTHEO',
				'PEAK.ERROR.PPM'
			)
		
		entry <- BiodbEntry$new(parent = .self)
		entry$setField('ACCESSION', jsontree$id)
		
		######################
		# TREATING THE PEAKS #
		######################
		
		entry$setField('NB.PEAKS', length(jsontree$peaks))
		peaks <- data.frame(matrix(0, ncol = length(cnames), nrow = 0))
		colnames(peaks) <- cnames
		###Parsing peaks.
		if (length(jsontree$peaks) != 0) {
			peaks <- sapply(jsontree$peaks, function(x) {
				return(
					list(
						as.double(x$mz),
						as.integer(x$ri),
						as.character(x$composition),
						as.double(x$theoricalMass),
						as.double(x$deltaPPM)
					)
				)
			})
			###Removing all whitespaces from the formule.
			peaks[3, ] <- vapply(peaks[3, ], function(x) {
				gsub(" ", "", trimws(x))
			}, FUN.VALUE = NA_character_)
			
			peaks <- as.data.frame(t(peaks))
			colnames(peaks) <- cnames
		}
		entry$setField('PEAKS', peaks)
		
		entries[[i]] <- entry
	}
	
	if (drop && length(content) == 1)
		entries <- entries[[1]]
	
	return(entries)
})

# Webservice lcms/peaks/get-range {{{1
################################################################

PeakforestMassConn$methods( wsLcmsPeaksGetRange = function(mz.min, mz.max, mode = NA_character_, biodb.ids = FALSE) {
	return(.self$.peaksGetRange('lcms', mz.min = mz.min, mz.max = mz.max, mode = mode, biodb.ids = biodb.ids))
})

# Webservice lcmsms/peaks/get-range {{{1
################################################################

PeakforestMassConn$methods( wsLcmsmsPeaksGetRange = function(mz.min, mz.max, mode = NA_character_, biodb.ids = FALSE) {
	return(.self$.peaksGetRange('lcmsms', mz.min = mz.min, mz.max = mz.max, mode = mode, biodb.ids = biodb.ids))
})

# Webservice lcmsms/from-precursor
################################################################

PeakforestMassConn$methods( wsLcmsmsFromPrecursor = function(prec.mz, precursorMassDelta, mode = NA_character_, biodb.ids = FALSE) {

	results <- character()

	if ( ! is.null(prec.mz) && ! is.na(prec.mz)) {

		# Build request
		url <- paste0(.self$getBaseUrl(), "spectra/lcmsms/from-precursor/", prec.mz)
		param <- c(token = .self$getToken(), precursorMassDelta = precursorMassDelta)
		if ( ! is.na(mode))
			param <- c(param, mode = mode)

		# Send request
		results <- .self$.getUrlScheduler()$getUrl(url, param = param)

		# Parse IDs
		if (biodb.ids)
			results <- .self$.parseIDsFromJson(results)
	}

	return(results)
})

# Get chromatographic columns {{{1
################################################################

PeakforestMassConn$methods( getChromCol = function(ids = NULL) {

	# Set URL
	url <- paste(.self$getBaseUrl(), 'metadata/lc/list-code-columns?token=', .self$getToken(), sep = '')

	# Send request
	json.str <- .self$.getUrlScheduler()$getUrl(url)
	wscols <- jsonlite::fromJSON(json.str, simplifyDataFrame = FALSE)

	# Build data frame
	cols <- data.frame(id = character(), title = character())
	for(id in names(wscols)) {
		col <- wscols[[id]]

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
		url <- paste(.self$getBaseUrl(), 'spectra/lcms/peaks/list-mz?token=', .self$getToken(), sep = '')
		if ( ! is.na(ms.mode))
			url <- paste(url, '&mode=', if (ms.mode == 'pos') 'positive' else 'negative', sep ='')

		# Get MZ values
		json.str <- .self$.getUrlScheduler()$getUrl(url)
		.self$.checkIfError(json.str)

		# Parse JSON
		mz <- jsonlite::fromJSON(json.str, simplifyDataFrame = FALSE)
	}

	# Apply cut-off
	if ( ! is.na(max.results) && length(mz) > max.results)
		mz <- mz[1:max.results]

	return(mz)
})

# Parse IDs from JSON {{{2
################################################################

PeakforestMassConn$methods( .parseIDsFromJson = function(json, json.is.parsed = FALSE) {

	ids <- NULL

	if ( ! json.is.parsed)
		json <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)

	if(length(json) > 0) {

		ids <- vapply(json, function(x) if ('id' %in% names(x)) x$id else (if ('source' %in% names(x) && ! is.null(x$source) && 'id' %in% names(x$source)) x$source$id else NA_integer_), FUN.VALUE = 1)
		ids <- ids[ ! is.na(ids)] # Remove NA values (some returned matches have no source information).
		ids <- unlist(ids)
		ids <- as.character(ids)
	}

	return(ids)
})

# Peaks get range {{{2
################################################################

PeakforestMassConn$methods( .peaksGetRange = function(spectra.type, mz.min, mz.max, mode = NA_character_, biodb.ids = FALSE) {
	                           
	results <- character()

	if ( ! is.null(mz.min) && ! is.null(mz.max) && ! is.na(mz.min) && ! is.na(mz.max)) {

		# Build request
		url <- paste0(.self$getBaseUrl(), "spectra/", spectra.type, "/peaks/get-range/", mz.min, "/", mz.max)
		param <- c(token = .self$getToken())
		if ( ! is.na(mode))
			param <- c(param, mode = mode)

		# Send request
		results <- .self$.getUrlScheduler()$getUrl(url, param = param)

		# Parse IDs
		if (biodb.ids)
			results <- .self$.parseIDsFromJson(results)
	}

	return(results)
})

# Get parsing expressions {{{2
################################################################

PeakforestMassConn$methods( .getParsingExpressions = function() {
	return(.BIODB.PEAKFOREST.MASS.PARSING.EXPR)
})

# Do search M/Z range {{{1
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
			ids <- .self$wsLcmsPeaksGetRange(mz.min, mz.max, mode = mode, biodb.ids = TRUE)
		if (ms.level == 0 || ms.level == 2) {
			if (precursor)
				ids <- c(ids, .self$wsLcmsmsFromPrecursor((mz.min + mz.max) / 2, (mz.max - mz.min) / 2, mode = mode, biodb.ids = TRUE))
			else
				ids <- c(ids, .self$wsLcmsmsPeaksGetRange(mz.min, mz.max, mode = mode, biodb.ids = TRUE))
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
		ids <- ids[1:max.results]

	return(ids)
})
