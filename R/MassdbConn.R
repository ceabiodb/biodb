# vi: fdm=marker

# Class declaration {{{1
################################################################

#' The mother class of all Mass spectra databases.
#'
#' All Mass spectra databases inherit from this class. It thus defines methods specific to mass spectrometry.
#'
#' @param compound.ids  A list of compound identifiers (i.e.: accession numbers). Used to filter the output. 
#' @param dist.fun      The distance function used to compute the distance betweem two mass spectra.
#' @param max.results   The maximum of elements returned by a method.
#' @param min.rel.int   The minimum relative intensity.
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

MassdbConn$methods( getChromCol = function(compound.ids = NULL) {
	":\n\nGet a list of chromatographic columns contained in this database. You can filter on specific compounds using the compound.ids parameter. The returned value is a data.frame with two columns : one for the ID 'id' and another one for the title 'title'."

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

MassdbConn$methods( getNbPeaks = function(mode = NULL, compound.ids = NULL) {
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

# MS-MS search {{{1
################################################################

MassdbConn$methods( msmsSearch = function(spectrum, precursor.mz, mz.tol, mz.tol.unit = BIODB.MZTOLUNIT.PLAIN, ms.mode = BIODB.MSMODE.POS, npmin = 2, dist.fun = BIODB.MSMS.DIST.WCOSINE, msms.mz.tol = 3, msms.mz.tol.min = 0.005) {
	":\n\nSearch MSMS spectra matching a template spectrum. The mz.tol parameter is applied on the precursor search."
	
	peak.tables <- list()

	# Get spectra IDs
	ids <- character()
	if ( ! is.null(spectrum) && nrow(spectrum) > 0 && ! is.null(precursor.mz))
		ids <- .self$searchMzTol(mz = precursor.mz, mz.tol = mz.tol, mz.tol.unit = mz.tol.unit, ms.mode = ms.mode, precursor = TRUE, ms.level = 2)

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

# Do search M/Z with tolerance {{{2
################################################################

MassdbConn$methods( .doSearchMzTol = function(mz, mz.tol, mz.tol.unit, min.rel.int, ms.mode, max.results, precursor, ms.level) {

	if (mz.tol.unit == BIODB.MZTOLUNIT.PPM)
		mz.tol <- mz.tol * mz * 1e-6

	mz.min <- mz - mz.tol
	mz.max <- mz + mz.tol

	return(.self$searchMzRange(mz.min = mz.min, mz.max = mz.max, min.rel.int = min.rel.int, ms.mode = ms.mode, max.results = max.results, precursor = precursor, ms.level = ms.level))
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
