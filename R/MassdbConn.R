# vi: fdm=marker

# Class declaration {{{1
################################################################

#' The mother class of all Mass spectra databases.
#'
#' All Mass spectra databases inherit from this class. It thus defines methods specific to mass spectrometry.
#'
#' @param ids           A list of entry identifiers (i.e.: accession numbers). Used to restrict the set of entries on which to run the algorithm.
#' @param chrom.col.ids IDs of chromatographic columns on which to match the retention time.
#' @param dist.fun      The distance function used to compute the distance betweem two mass spectra.
#' @param max.results   The maximum of elements returned by a method.
#' @param min.rel.int   The minimum relative intensity, in percentage (i.e.: float number between 0 and 100).
#' @param ms.level      The MS level to which you want to restrict your search. \code{0} means that you want to serach in all levels.
#' @param ms.mode       The MS mode. Set it to either \code{BIODB.MSMODE.NEG} or \code{BIODB.MSMODE.POS}.
#' @param msms.mz.tol       M/Z tolerance to apply while matching MSMS spectra. In PPM.
#' @param msms.mz.tol.min   Minimum of the M/Z tolerance (plain unit). If the M/Z tolerance computed with \code{msms.mz.tol} is lower than \code{msms.mz.tol.min}, then \code{msms.mz.tol.min} will be used.
#' @param mz            An M/Z value.
#' @param mz.max        The maximum allowed for searched M/Z values.
#' @param mz.min        The minimum allowed for searched M/Z values.
#' @param mz.tol        The M/Z tolerance, whose unit is defined by \code{mz.tol.unit}.
#' @param mz.tol.unit   The unit of the M/Z tolerance. Set it to either \code{'ppm'} or \code{'plain'}.
#' @param npmin         The minimum number of peak to detect a match (2 is recommended).
#' @param precursor     If set to \code{TRUE}, then restrict the search to precursor peaks.
#' @param precursor.mz  The M/Z value of the precursor peak of the mass spectrum.
#' @param spectrum      A template spectrum to match inside the database.
#' @param rts           Retention times to match. Unit is specified by rt.unit parameter.
#' @param rt.unit       The unit for submitted retention times. Either 's' or 'min'.
#' @param rt.tol        The plain tolerance for retention times: rt - rt.tol <= input.rt <= rt + rt.tol. Unit is the same as rts, and is specified by rt.unit parameter.
#' @param rt.tol.exp    A special exponent tolerance for retention times: rt - rt.tol - rt ** rt.tol.exp <= input.rt <= rt + rt.tol + rt ** rt.tol.exp. This exponent is applied on the RT value in seconds.
#'
#' @seealso \code{\link{BiodbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get connector
#' conn <- mybiodb$getFactory()$createConn('massbank.jp')
#'
#'
#' @import methods
#' @include BiodbConn.R
#' @export MassdbConn
#' @exportClass MassdbConn
MassdbConn <- methods::setRefClass("MassdbConn", contains = "BiodbConn")

# Constructor {{{1
################################################################

MassdbConn$methods( initialize = function(...) {

	callSuper(...)
	.self$.abstract.class('MassdbConn')
})

# Get chromatographic columns {{{1
################################################################

MassdbConn$methods( getChromCol = function(ids = NULL) {
	":\n\nGet a list of chromatographic columns contained in this database. You can filter on specific entries using the ids parameter. The returned value is a data.frame with two columns : one for the ID 'id' and another one for the title 'title'."

	.self$.abstract.method()
})

# Get mz values {{{1
################################################################

MassdbConn$methods( getMzValues = function(ms.mode = NA_character_, max.results = NA_integer_, precursor = FALSE, ms.level = 0) {
	":\n\nGet a list of M/Z values contained inside the database."

	.self$.doGetMzValues(ms.mode = ms.mode, max.results = max.results, precursor = precursor, ms.level = ms.level)
})

# Get nb peaks {{{1
################################################################

MassdbConn$methods( getNbPeaks = function(mode = NULL, ids = NULL) {
	":\n\nReturns the number of peaks contained in the database."

	.self$.abstract.method()
})

# Search by M/Z within range {{{1
################################################################

MassdbConn$methods( searchMzRange = function(mz.min, mz.max, min.rel.int = NA_real_, ms.mode = NA_character_, max.results = NA_integer_, precursor = FALSE, ms.level = 0) {
	":\n\nFind spectra in the given M/Z range. Returns a list of spectra IDs."
	
	# Check arguments
	if ( ! .self$.assert.not.na(mz.min, msg.type = 'warning')) return(NULL)
	if ( ! .self$.assert.not.null(mz.max, msg.type = 'warning')) return(NULL)
	if ( ! .self$.assert.not.na(mz.min, msg.type = 'warning')) return(NULL)
	if ( ! .self$.assert.not.null(mz.max, msg.type = 'warning')) return(NULL)
	.self$.assert.positive(mz.min)
	.self$.assert.positive(mz.max)
	.self$.assert.inferior(mz.min, mz.max)
	.self$.assert.positive(min.rel.int)
	.self$.assert.in(ms.mode, BIODB.MSMODE.VALS)
	.self$.assert.positive(max.results)
	.self$.assert.positive(ms.level)
	if (length(mz.min) != length(mz.max))
		.self$message('error', 'mz.min and mz.max must have the same length in searchMzRange().')

	return(.self$.doSearchMzRange(mz.min = mz.min, mz.max = mz.max, min.rel.int = min.rel.int, ms.mode = ms.mode, max.results = max.results, precursor = precursor, ms.level = ms.level))
})

# Search by M/Z within tolerance {{{1
################################################################

MassdbConn$methods( searchMzTol = function(mz, mz.tol, mz.tol.unit = BIODB.MZTOLUNIT.PLAIN, min.rel.int = NA_real_, ms.mode = NA_character_, max.results = NA_integer_, precursor = FALSE, ms.level = 0) {
	":\n\nFind spectra containg a peak around the given M/Z value. Returns a character vector of spectra IDs."
	
	if ( ! .self$.assert.not.na(mz, msg.type = 'warning')) return(NULL)
	if ( ! .self$.assert.not.null(mz, msg.type = 'warning')) return(NULL)
	.self$.assert.positive(mz)
	.self$.assert.positive(mz.tol)
	.self$.assert.length.one(mz.tol)
	.self$.assert.in(mz.tol.unit, BIODB.MZTOLUNIT.VALS)
	.self$.assert.positive(min.rel.int)
	.self$.assert.in(ms.mode, BIODB.MSMODE.VALS)
	.self$.assert.positive(max.results)
	.self$.assert.positive(ms.level)

	return(.self$.doSearchMzTol(mz = mz, mz.tol = mz.tol, mz.tol.unit = mz.tol.unit, min.rel.int = min.rel.int, ms.mode = ms.mode, max.results = max.results, precursor = precursor, ms.level = ms.level))
})

# Search MS peaks {{{1
################################################################

MassdbConn$methods ( searchMsPeaks = function(mzs, mz.shift = 0.0, mz.tol, mz.tol.unit = BIODB.MZTOLUNIT.PLAIN, min.rel.int = NA_real_, ms.mode = NA_character_, ms.level = 0, max.results = NA_integer_, chrom.col.ids = NA_character_, rts = NA_real_, rt.unit = NA_character_, rt.tol = NA_real_, rt.tol.exp = NA_real_) {
	":\n\nFor each M/Z value, search for matching MS spectra and return the matching peaks. If max.results is set, it is used to limit the number of matches found for each M/Z value."

	# Check M/Z values
	if ( ! .self$.assert.not.na(mzs, msg.type = 'warning')) return(NULL)
	if ( ! .self$.assert.not.null(mzs, msg.type = 'warning')) return(NULL)
	.self$.assert.is(mzs, 'numeric')
	.self$.assert.positive(mzs)
	.self$.assert.positive(mz.tol)
	.self$.assert.length.one(mz.tol)
	.self$.assert.length.one(mz.shift)
	.self$.assert.in(mz.tol.unit, BIODB.MZTOLUNIT.VALS)

	# Check RT values
	match.rt <- ! is.null(rts) && ! all(is.na(rts))
	if (match.rt) {
		.self$.assert.is(rts, 'numeric')
		.self$.assert.equal.length(mzs, rts)
		.self$.assert.positive(rts)
		.self$.assert.not.null(chrom.col.ids)
		.self$.assert.not.na(chrom.col.ids)
		.self$.assert.is(chrom.col.ids, 'character')
		.self$.assert.not.na(rt.unit)
		.self$.assert.in(rt.unit, c('s', 'min'))
		.self$.assert.length.one(rt.unit)

#		# Convert input RT values in seconds
#		if (rt.unit != 's') {
#			rts <- .self$.convert.rt(rts, rep(rt.unit, length(rts)), rep('s', length(rts)))
#			rt.tol <- .self$.convert.rt(rt.tol, rt.unit, 's')
#		}
	}

	# Check other parameters
	.self$.assert.positive(min.rel.int)
	.self$.assert.in(ms.mode, .self$getBiodb()$getEntryFields()$get('ms.mode')$getAllowedValues())
	.self$.assert.positive(max.results)

	results <- NULL

	# Loop on the list of M/Z values
	.self$message('debug', 'Looping on all M/Z values.')
	for (i in seq_along(mzs)) {
		mz <- mzs[[i]]

		# Compute M/Z range
		mz.range <- .self$.mztolToRange(mz = mz, mz.shift = mz.shift, mz.tol = mz.tol, mz.tol.unit = mz.tol.unit)

		# Search for spectra
		.self$message('debug', paste('Searching for spectra that contains M/Z value ', mz, '.', sep = ''))
		ids <- .self$searchMzRange(mz.min = mz.range$min, mz.max = mz.range$max, min.rel.int = min.rel.int, ms.mode = ms.mode, max.results = if (match.rt) NA_integer_ else max.results, ms.level = ms.level)
		.self$message('debug', paste0('Found ', length(ids), ' spectra:', paste((if (length(ids) <= 10) ids else ids[1:10]), collapse = ', '), '.'))

		# Get entries
		.self$message('debug', 'Getting entries from spectra IDs.')
		entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), ids, drop = FALSE)

		# Select rows with matching RT values
		
		if  (match.rt) {

			rt <- rts[[i]]

			.self$message('debug', 'Filtering peaks list on RT values.')

			# Filtering on chromatographic columns
			entries <- entries[vapply(entries, function(e) e$getFieldValue('chrom.col.id') %in% chrom.col.ids, FUN.VALUE = TRUE)]
			.self$message('debug', paste0(length(entries), ' spectra remaining after chrom col filtering: ', paste(vapply((if (length(entries) <= 10) entries else entries[1:10]), function(e) e$getFieldValue('accession'), FUN.VALUE = ''), collapse = ', '), '.'))

			# Filter out entries with no RT values or no RT unit
			has.chrom.rt.values <- vapply(entries, function(e) {e$hasField('chrom.rt') || (e$hasField('chrom.rt.min') && e$hasField('chrom.rt.max'))}, FUN.VALUE = TRUE)
			entries <- entries[has.chrom.rt.values]
			if (sum( ! has.chrom.rt.values) > 0)
				.self$message('debug', paste('Filtered out', sum( ! has.chrom.rt.values), 'entries having no RT values.'))
			no.chrom.rt.unit <- ! vapply(entries, function(e) e$hasField('chrom.rt.unit'), FUN.VALUE = TRUE)
			if (any(no.chrom.rt.unit))
				.self$message('caution', paste0('No RT unit specified in entries ', paste(vapply(entries[no.chrom.rt.unit], function(e) e$getFieldValue('accession'), FUN.VALUE = ''), collapse = ', '), ', impossible to match retention times.'))
			
			# Filtering on retention time
			tmp <- list()
			for (e in entries) {

				# Get RT min and max for this column, in seconds
				rt.col.unit <- e$getFieldValue('chrom.rt.unit')
				if (e$hasField('chrom.rt')) {
					rt.col.min <- .self$.convert.rt(e$getFieldValue('chrom.rt'), rt.col.unit, 's')
					rt.col.max <- rt.col.min
				} else if (e$hasField('chrom.rt.min') && e$hasField('chrom.rt.max')) {
					rt.col.min <- .self$.convert.rt(e$getFieldValue('chrom.rt.min'), rt.col.unit, 's')
					rt.col.max <- .self$.convert.rt(e$getFieldValue('chrom.rt.max'), rt.col.unit, 's')
				} else
					.self$message('error', 'Impossible to match on retention time, no retention time fields (chrom.rt or chrom.rt.min and chrom.rt.max) were found.')

				# Compute RT range for this input, in seconds
				rt.sec <- .self$.convert.rt(rt, rt.unit, 's')
				rt.min <- rt.sec
				rt.max <- rt.sec
				if ( ! is.na(rt.tol)) {
					rt.tol.sec <- .self$.convert.rt(rt.tol, rt.unit, 's')
					rt.min <- rt.min - rt.tol.sec
					rt.max <- rt.max + rt.tol.sec
				}
				if ( ! is.na(rt.tol.exp)) {
					rt.min <- rt.min - rt.sec ** rt.tol.exp
					rt.max <- rt.max + rt.sec ** rt.tol.exp
				}

				# Test and possibly keep entry
				.self$message('debug', paste0('Testing if RT value ', rt, ' (', rt.unit, ') is in range [', rt.col.min, ';', rt.col.max, '] (', rt.col.unit, ') of database entry ', e$getFieldValue('accession'), '. Used range (after applying tolerances) for RT value is [', rt.min, ', ', rt.max, '] (s).'))
				if ((rt.max >= rt.col.min) && (rt.min <= rt.col.max))
					tmp <- c(tmp, e)
			}
			entries <- tmp
			.self$message('debug', paste0(length(entries), ' spectra remaining after retention time filtering:', paste(vapply((if (length(entries) <= 10) entries else entries[1:10]), function(e) e$getFieldValue('accession'), FUN.VALUE = ''), collapse = ', '), '.'))
		}

		# Cut
		if ( ! is.na(max.results) && length(entries) > max.results) {
			.self$message('debug', paste('Cutting data frame', max.results, 'rows.'))
			entries <- entries[1:max.results]
		}
		if (length(entries) > 0)
			.self$message('debug', paste('Field names of first entry:', paste(entries[[1]]$getFieldNames(), collapse = ', ')))

		# Convert to data frame
		.self$message('debug', 'Converting list of entries to data frame.')
		df <- .self$getBiodb()$entriesToDataframe(entries, only.atomic = FALSE)
		.self$message('debug', paste('Data frame contains', nrow(df), 'rows.'))
		
		# Select lines with right M/Z values
		mz.range <- .self$.mztolToRange(mz = mz, mz.shift = mz.shift, mz.tol = mz.tol, mz.tol.unit = mz.tol.unit)
		.self$message('debug', paste("Filtering entries data frame on M/Z range [", mz.range$min, ', ', mz.range$max, '].', sep = ''))
		df <- df[(df$peak.mz >= mz.range$min) & (df$peak.mz <= mz.range$max), ]
		.self$message('debug', paste('Data frame contains', nrow(df), 'rows.'))

		.self$message('debug', 'Merging data frame of matchings into results data frame.')
		results <- plyr::rbind.fill(results, df)
		.self$message('debug', paste('Total results data frame contains', nrow(results), 'rows.'))
	}

	return(results)
})

# MS-MS search {{{1
################################################################

MassdbConn$methods( msmsSearch = function(spectrum, precursor.mz, mz.tol, mz.tol.unit = 'plain', ms.mode, npmin = 2, dist.fun = BIODB.MSMS.DIST.WCOSINE, msms.mz.tol = 3, msms.mz.tol.min = 0.005, max.results = NA_integer_) {
	":\n\nSearch MSMS spectra matching a template spectrum. The mz.tol parameter is applied on the precursor search."
	
	peak.tables <- list()

	# Get spectra IDs
	ids <- character()
	if ( ! is.null(spectrum) && nrow(spectrum) > 0 && ! is.null(precursor.mz)) {
		if ( ! is.na(max.results))
			.self$message('caution', paste('Applying max.results =', max.results,'on call to searchMzTol(). This may results in no matches, while there exist matching spectra inside the database.'))
		ids <- .self$searchMzTol(mz = precursor.mz, mz.tol = mz.tol, mz.tol.unit = mz.tol.unit, ms.mode = ms.mode, precursor = TRUE, ms.level = 2, max.results = max.results)
	}

	# Get list of peak tables from spectra
	if (length(ids) > 0) {
		entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), ids, drop = FALSE)
		peak.tables <- lapply(entries, function(x) x$getFieldsAsDataFrame(only.atomic = FALSE, fields = 'PEAKS'))
	}

	# Compare spectrum against database spectra
	res <- compareSpectra(spectrum, peak.tables, npmin = npmin, fun = dist.fun, params = list(ppm = msms.mz.tol, dmz = msms.mz.tol.min))
	
	#if(is.null(res)) return(NULL) # To decide at MassdbConn level: return empty list (or empty data frame) or NULL.
	
    cols <- colnames(res)
    res[['id']] <- ids
    res <- res[, c('id', cols)]
	
    # Order rows
    res <- res[order(res[['score']], decreasing = TRUE), ]

#    results <- list(measure = res$similarity[res[['ord']]], matchedpeaks = res$matched[res[['ord']]], id = ids[res[['ord']]])
	
	return(res)
})

# Get entry ids {{{1
################################################################

MassdbConn$methods( getEntryIds = function(max.results = NA_integer_, ms.level = 0) {
	":\n\nGet entry identifiers from the database."

	.self$.abstract.method()
})

# PRIVATE METHODS {{{1
################################################################

MassdbConn$methods( .mztolToRange = function(mz, mz.shift, mz.tol, mz.tol.unit) {

	if (mz.tol.unit == BIODB.MZTOLUNIT.PPM) {
		mz.min <- mz + mz * ( mz.shift - mz.tol) * 1e-6
		mz.max <- mz + mz * ( mz.shift + mz.tol) * 1e-6
	}
	else {
		mz.min <- mz + mz.shift - mz.tol
		mz.max <- mz + mz.shift + mz.tol
	}


	return(list(min = mz.min, max = mz.max))
})

# Do search M/Z with tolerance {{{2
################################################################

MassdbConn$methods( .doSearchMzTol = function(mz, mz.tol, mz.tol.unit, min.rel.int, ms.mode, max.results, precursor, ms.level) {

	range <- .self$.mztolToRange(mz = mz, mz.shift = 0.0, mz.tol = mz.tol, mz.tol.unit = mz.tol.unit)

	return(.self$searchMzRange(mz.min = range$min, mz.max = range$max, min.rel.int = min.rel.int, ms.mode = ms.mode, max.results = max.results, precursor = precursor, ms.level = ms.level))
})

# Do search M/Z range {{{2
################################################################

MassdbConn$methods( .doSearchMzRange = function(mz.min, mz.max, min.rel.int, ms.mode, max.results, precursor, ms.level) {
	.self$.abstract.method()
})

# Do get mz values {{{2
################################################################

MassdbConn$methods( .doGetMzValues = function(ms.mode, max.results, precursor, ms.level) {
	.self$.abstract.method()
})

# Convert RT values {{{2

################################################################

MassdbConn$methods( .convert.rt = function(rts, units, wanted.unit) {

	# RT values with wrong unit
	rts.wrong <- units != wanted.unit

	# Convert any RT value using wrong unit
	if (any(rts.wrong)) {
		if ('s' %in% units[rts.wrong]) {
			if (wanted.unit != 'min')
				.self$message('error', 'Error when converting retention times values. Was expecting "min" for target unit.')
			rts[rts.wrong] <- rts[rts.wrong] / 60
		}
		if ('min' %in% units[rts.wrong]) {
			if (wanted.unit != 's')
				.self$message('error', 'Error when converting retention times values. Was expecting "s" for target unit.')
			rts[rts.wrong] <- rts[rts.wrong] * 60
		}
	}

	return(rts)
})
