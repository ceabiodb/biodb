# vi: fdm=marker

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

# Get entry content url {{{1
################################################################

PeakforestMassConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {

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
	return(paste('https://peakforest.org/home?PFs=', id))
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

	# Build request
	url <- paste0(.self$getBaseUrl(), "spectra/lcmsms/from-precursor/", prec.mz)
	param <- c(token = .self$getToken(), precursorMassDelta = precursorMassDelta)
	if ( ! is.na(mode))
		param <- c(mode = mode)

	# Send request
	results <- .self$.getUrlScheduler()$getUrl(url, param = param)

	# Parse IDs
	if (biodb.ids)
		results <- .self$.parseIDsFromJson(results)

	return(results)
})

# Do search M/Z range {{{1
################################################################

PeakforestMassConn$methods( .doSearchMzRange = function(mz.min, mz.max, min.rel.int, ms.mode, max.results, precursor, ms.level) {
	
	# Search for spectra with this M/Z value
	ids <- NULL
	mode <- if ( ! is.na(ms.mode)) (if (ms.mode == 'neg') 'NEG' else 'POS') else NA_character_
	if (ms.level == 0 || ms.level == 1)
		ids <- .self$wsLcmsPeaksGetRange(mz.min, mz.max, mode = mode, biodb.ids = TRUE)
	if (ms.level == 0 || ms.level == 2) {
		if (precursor)
			ids <- c(ids, .self$wsLcmsmsFromPrecursor((mz.min + mz.max) / 2, (mz.max - mz.min) / 2, mode = mode, biodb.ids = TRUE))
		else
			ids <- c(ids, .self$wsLcmsmsPeaksGetRange(mz.min, mz.max, mode = mode, biodb.ids = TRUE))
	}

	# Filtering
	if ( length(ids) > 0 && ( ! is.na(min.rel.int) || precursor)) {

		entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), ids)

		# Filter on precursor
		if (precursor) {
			entries <- lapply(entries, function(e) if (is.null(e) || ! e$hasField('msprecmz') || e$getFieldValue('msprecmz') < mz.min || e$getFieldValue('msprecmz') > mz.max) NULL else e)
		}

		# Filter on intensity
		if ( ! is.na(min.rel.int))
			entries <- lapply(entries, function(e) if (is.null(e) || ! e$hasField('peaks') || any(e$hasField('peaks')[e$hasField('peaks')['peak.mz'] >= mz.min & e$hasField('peaks')['peak.mz'] <= mz.max, 'peak.relative.intensity'] < min.rel.int)) NULL else e)

		# Select entries
		ids <- ids[ ! vapply(entries, is.null, FUN.VALUE = TRUE)]
	}

	# Cut
	if ( ! is.na(max.results) && length(ids) > max.results)
		ids <- ids[1:max.results]

	return(ids)
})

# Get chromatographic columns {{{1
################################################################

PeakforestMassConn$methods( getChromCol = function(compound.ids = NULL) {

	# Set URL
	url <- paste(.self$getBaseUrl(), 'metadata/lc/list-code-columns?token=', .self$getToken(), sep = '')
	if ( ! is.null(compound.ids))
		url <- paste(url, '&molids=', paste(compound.ids, collapse = ','), sep = '')

	# Send request
	wscols <- .self$.getUrlScheduler$getUrl(url)

	# Build data frame
	cols <- data.frame(id = character(), title = character())
	for(id in names(wscols))
		cols <- rbind(cols, data.frame(id = id, title = wscols[[id]]$name, stringsAsFactors = FALSE))

	return(cols)
})

# PRIVATE METHODS {{{1
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
			url <- paste(url, '&mode=', if (ms.mode == BIODB.MSMODE.POS) 'positive' else 'negative', sep ='')

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
	                           
	# Build request
	url <- paste0(.self$getBaseUrl(), "spectra/", spectra.type, "/peaks/get-range/", mz.min, "/", mz.max)
	param <- c(token = .self$getToken())
	if ( ! is.na(mode))
		param <- c(mode = mode)

	# Send request
	results <- .self$.getUrlScheduler()$getUrl(url, param = param)

	# Parse IDs
	if (biodb.ids)
		results <- .self$.parseIDsFromJson(results)

	return(results)
})
