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

# Do search M/Z range {{{1
################################################################

PeakforestMassConn$methods( .doSearchMzRange = function(mz.min, mz.max, min.rel.int, ms.mode, max.results, precursor, ms.level) {
	
	url <- paste0(.self$getBaseUrl(), "spectra/lcms/peaks/get-range/", mz.min, "/", mz.max, '?token=', .self$getToken())
	
	# Send request
	content <- .self$.getUrlScheduler()$getUrl(url)
	jsontree <- jsonlite::fromJSON(content, simplifyDataFrame = FALSE)
	
	# No match form the output.
	if(length(jsontree) == 0)
		return(NULL)

	# Getting a list of all the IDs.
	ids <- vapply(jsontree, function(x) if ('source' %in% names(x) && ! is.null(x$source) && 'id' %in% names(x$source)) x$source$id else NA_integer_, FUN.VALUE = 1)
	ids <- ids[ ! is.na(ids)] # Remove NA values (some returned matches have no source information).
	ids <- unlist(ids)
	ids <- as.character(ids)

	# Cut
	if ( ! is.na(max.results) && length(ids) > max.results)
		ids <- ids[1:max.results]

	return(ids)
})

# Do search precursor within tolerance {{{1
################################################################

# DEPRECATED
# TODO rename this method and use it inside .doSearchMzRange using new params precursor and level.
PeakforestMassConn$methods( .doSearchPrecTol = function(prec.mz, mz.tol, mz.tol.unit = BIODB.MZTOLUNIT.PLAIN, ms.mode = NA_character_) {

	# Set token
	largs <- list(token = .self$getToken())
	
	# Set mode
	if ( ! is.na(ms.mode) && ms.mode %in% c(BIODB.MSMODE.NEG, BIODB.MSMODE.POS))
			largs <- c(largs, mode = ms.mode)
	
	# Set tolerance
	if (mz.tol.unit == BIODB.MZTOLUNIT.PPM)
		mz.tol <- mz.tol * prec.mz * 10 ^ -6
	largs <- c(largs, precursorMassDelta = mz.tol)

	strargs <- apply(rbind(names(largs),as.character(largs)),2,paste,collapse="=")
	strargs <- paste(strargs,collapse = "&")
	##Request which return peak and not spectra.
	url <- paste0(.self$getBaseUrl(), "spectra/lcmsms/from-precursor/", prec.mz, "?", strargs)
	contents <-  .self$.get.url(url)
	# TODO Save contents in cache so it's faster next time?
	# Get IDs from contents
#	entries  <- .self$createReducedEntry(contents, drop = FALSE)

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

# Get mz values {{{1
################################################################

PeakforestMassConn$methods( .doGetMzValues = function(ms.mode, max.results, precursor, ms.level) {

	mz <- NULL

	if (ms.level > 0 || precusor) {

		# Get all IDs
		ids <- .self$getEntryIds()
		print('-------------------------------- PeakforestMassConn::.doGetMzValues 10')
		print(length(ids))

		# Loop on all IDs
		for (id in ids) {
			print('-------------------------------- PeakforestMassConn::.doGetMzValues 11')

			print(id)
			entry <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), id)
			print(entry$getFieldValue('ms.level'))
			print(ms.level)

			if (ms.level > 0 && ( ! entry$hasField('ms.level') || entry$getFieldValue('ms.level') != ms.level))
				next
			print('-------------------------------- PeakforestMassConn::.doGetMzValues 14')

			if (precursor) {
				if (entry$hasField('msprecmz'))
					mz <- c(mz, entry$getFieldValue('msprecmz'))
			}
			else {
				if (entry$hasField('peaks'))
					mz <- c(mz, entry$getFieldValue('peaks')[['PEAK.MZ']])
			}
			print(length(mz))

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
