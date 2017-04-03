# vi: fdm=marker

# Class declaration {{{1
################################################################

#' The mother class of all Mass spectra databases.
#'
#' All Mass spectra databases inherit from this class.
#'
#' @param max.results   The maximum of elements returned by a method.
#' @param min.rel.int   The minimum relative intensity.
#' @param ms.level      The MS level to which you want to restrict your search. \code{0} means that you want to serach in all levels.
#' @param ms.mode       The MS mode. Set it to either \code{BIODB.MSMODE.NEG} or \code{BIODB.MSMODE.POS}.
#' @param mz            An M/Z value.
#' @param mz.max        The maximum allowed for searched M/Z values.
#' @param mz.min        The minimum allowed for searched M/Z values.
#' @param plain.tol     The M/Z tolerance in M/Z unit. Thus the searched mz must satisfy the folloing double inequality: mz - plain.tol <= mz <= mz + plain.tol.
#' @param precursor     If set to \code{TRUE}, then restrict the search to precursor peaks.
#' @param tol           An M/Z tolerance, whose unit is not defined.
#' @param tol.unit      The unit of the M/Z tolerance. Set it to either \code{BIODB.MZTOLUNIT.PPM} or \code{BIODB.MZTOLUNIT.PLAIN}.
#'
#' @import methods
#' @include BiodbConn.R
MassdbConn <- methods::setRefClass("MassdbConn", contains = "BiodbConn")

# Get chromatographic columns {{{1
################################################################

# Get a list of chromatographic columns contained in this database.
# compound.ids  A list of compound IDs used to filter results.
# The returned value is a data.frame with two columns : one for the ID (BIODB.ID) and another one for the title (BIODB.TITLE).
MassdbConn$methods( getChromCol = function(compound.ids = NULL) {
	.self$.abstract.method()
})

# Get mz values {{{1
################################################################

# Returns a numeric vector of all masses stored inside the database.
MassdbConn$methods( getMzValues = function(ms.mode = NA_character_, max.results = NA_integer_, precursor = FALSE, level = 0) {
	"Get a list of M/Z values contained inside the database."

	.self$.doGetMzValues(ms.mode = ms.mode, max.results = max.results, precursor = precursor, level = level)
})

# Do get mz values {{{1
################################################################

MassdbConn$methods( .doGetMzValues = function(ms.mode, max.results, precursor, level) {
	.self$.abstract.method()
})
# Get nb peaks {{{1
################################################################

# Returns the number of peaks contained in the database
MassdbConn$methods( getNbPeaks = function(mode = NULL, compound.ids = NULL) {
	.self$.abstract.method()
})

# Find compound by name {{{1
################################################################

# Find a molecule by name
# name   A vector of molecule names to search for.
# Return an integer vector of the same size as the name input vector, containing the found molecule IDs, in the same order.
# TODO Rename it searchByCompound
MassdbConn$methods( findCompoundByName = function(name) {
	.self$.abstract.method()
})

# Search peak {{{1
#################################################################
#
#MassdbConn$methods( searchPeak = function(mz = NA_real_, plain.tol = NA_real_, relint = NA_integer_, mode = NA_character_, max.results = NA_integer_) {
#	"Search matching peaks in database, and return a list of entry IDs."
#
#	if (is.na(mz) || length(mz) == 0)
#		.self$message(MSG.ERROR, "At least one mz value is required for searchPeak() method.")
#	if (! all(mz > 0))
#		.self$message(MSG.ERROR, "MZ values must be positive for searchPeak() method.")
#	if (is.na(tol) || length(tol) == 0)
#		.self$message(MSG.ERROR, "A tolerance value is required for searchPeak() method.")
#	if (length(tol) > 1)
#		.self$message(MSG.ERROR, "No more than one tolerance value is allowed for searchPeak() method.")
#	if ( ! is.na(max.results) && max.results < 0)
#		.self$message(MSG.ERROR, "The maximum number of results must be positive or zero for searchPeak() method.")
#	if ( ! is.na(mode) && ! mode %in% c(BIODB.MSMODE.NEG, BIODB.MSMODE.POS))
#		.self$message(MSG.ERROR, paste("Incorrect MS mode", mode, "for searchPeak() method."))
#	if ( ! is.na(relint) && relint < 0)
#		.self$message(MSG.ERROR, "The relative intensity must be positive or zero for searchPeak() method.")
#
#	.self$.do.search.peak(mz = mz, plain.tol = plain.tol, relint = relint, mode = mode, max.results = max.results)
#})
#
# Do search peak {{{1
#################################################################
#
#MassdbConn$methods( .do.search.peak = function(mz = NA_real_, plain.tol = NA_real_, relint = NA_integer_, mode = NA_character_, max.results = NA_integer_) {
#	.self$.abstract.method()
#})

# Search by M/Z within range {{{1
################################################################

MassdbConn$methods( searchMzRange = function(mz.min, mz.max, min.rel.int = NA_real_, ms.mode = NA_character_, max.results = NA_integer_, precursor = FALSE, ms.level = 0) {
	"Find spectra in the given M/Z range. Returns a list of spectra IDs."
	
	# Check arguments
	if ( ! .self$.assert.not.na(mz.min, msg.type = MSG.WARNING)) return(NULL)
	if ( ! .self$.assert.not.null(mz.max, msg.type = MSG.WARNING)) return(NULL)
	if ( ! .self$.assert.not.na(mz.min, msg.type = MSG.WARNING)) return(NULL)
	if ( ! .self$.assert.not.null(mz.max, msg.type = MSG.WARNING)) return(NULL)
	.self$.assert.positive(mz.min)
	.self$.assert.positive(mz.max)
	.self$.assert.length.one(mz.min)
	.self$.assert.length.one(mz.max)
	.self$.assert.inferior(mz.min, mz.max)
	.self$.assert.positive(min.rel.int)
	.self$.assert.in(ms.mode, BIODB.MSMODE.VALS)
	.self$.assert.positive(max.results)
	.self$.assert.positive(ms.level)

	return(.self$.doSearchMzRange(mz.min = mz.min, mz.max = mz.max, min.rel.int = min.rel.int, ms.mode = ms.mode, max.results = max.results, precursor = precursor, ms.level = ms.level))
})

# Do search M/Z range {{{1
################################################################

MassdbConn$methods( .doSearchMzRange = function(mz.min, mz.max, min.rel.int, ms.mode, max.results, precursor, ms.level) {
	.self$.abstract.method()
})

# Search by M/Z within tolerance {{{1
################################################################

MassdbConn$methods( searchMzTol = function(mz, tol, tol.unit = BIODB.MZTOLUNIT.PLAIN, min.rel.int = NA_real_, ms.mode = NA_character_, max.results = NA_integer_, precursor = FALSE, ms.level = 0) {
	"Find spectra containg a peak around the given M/Z value. Returns a list of spectra IDs."
	
	if ( ! .self$.assert.not.na(mz, msg.type = MSG.WARNING)) return(NULL)
	if ( ! .self$.assert.not.null(mz, msg.type = MSG.WARNING)) return(NULL)
	.self$.assert.positive(mz)
	.self$.assert.length.one(mz)
	.self$.assert.positive(tol)
	.self$.assert.length.one(tol)
	.self$.assert.in(tol.unit, BIODB.MZTOLUNIT.VALS)
	.self$.assert.positive(min.rel.int)
	.self$.assert.in(ms.mode, BIODB.MSMODE.VALS)
	.self$.assert.positive(max.results)
	.self$.assert.positive(ms.level)

	return(.self$.doSearchMzTol(mz = mz, tol = tol, tol.unit = tol.unit, min.rel.int = min.rel.int, ms.mode = ms.mode, max.results = max.results, precursor = precursor, ms.level = ms.level))
})

# Do search M/Z with tolerance {{{1
################################################################

MassdbConn$methods( .doSearchMzTol = function(mz, tol, tol.unit, min.rel.int, ms.mode, max.results, precursor, ms.level) {

	if (tol.unit == BIODB.MZTOLUNIT.PPM)
		tol <- tol * mz * 1e-6

	mz.min <- mz - tol
	mz.max <- mz + tol

	return(.self$searchMzRange(mz.min = mz.min, mz.max = mz.max, min.rel.int = min.rel.int, ms.mode = ms.mode, max.results = max.results, precursor = precursor, ms.level = ms.level))
})

# MS-MS search {{{1
################################################################

### spec : the spec to match against the database.
### precursor : the mass/charge of the precursor to be looked for.
### mtol : the size of the windows arounf the precursor to be looked for.
### ppm : the matching ppm tolerance.
### fun :  
### dmz : the mass tolerance is taken as the minium between this quantity and the ppm.
### npmin : the minimum number of peak to detect a match (2 recommended)

MassdbConn$methods( msmsSearch = function(spec, precursor, mztol, tolunit,
											 ppm, fun = BIODB.MSMS.DIST.WCOSINE,
											 params = list(), npmin=2, dmz = 0.001,
											 mode = BIODB.MSMODE.POS, return.ids.only = TRUE){

	
	# TODO replace by msms precursor search when available.
	writetc <- textConnection("spec.str", "w", local = TRUE)
	write.csv(spec, writetc)
	.self$message(MSG.DEBUG, spec.str)
	.self$message(MSG.DEBUG, precursor)

	# Now returns a list of IDs, needs to get the peak tables from that
	lspec <- .self$searchMzTol(mz = precursor, tol = mztol, tol.unit = BIODB.MZTOLUNIT.PLAIN, ms.mode = mode, precursor = TRUE, ms.level = 2)

	.self$message(MSG.DEBUG, class(lspec))
	if(length(lspec)==0){
		return(list(measure = numeric(0), matchedpeaks = list(), id = character(0)))
	}
	rspec <- lapply(lspec,function(x){
        peaks <- x$getFieldValue(BIODB.PEAKS,compute=FALSE)
		####Getting the correct fields
		vcomp <- c(BIODB.PEAK.MZ, BIODB.PEAK.RELATIVE.INTENSITY, BIODB.PEAK.INTENSITY)
		
		foundfields <- vcomp %in% colnames(peaks)
		if(sum(foundfields ) < 2){
			.self$message(MSG.ERROR, paste0("fields can't be coerced to mz and intensity : ",colnames(peaks)))
		}
		
		peaks <- peaks[ , vcomp[which( foundfields ) ] ]
		
		peaks
	})
	
	# TODO Import compareSpectra into biodb and put it inside massdb-helper.R or hide it as a private method.
	res <- compareSpectra(spec, rspec, npmin = npmin, fun = fun, params = params)
	
	if(is.null(res)) return(NULL) # To decide at MassdbConn level: return empty list (or empty data frame) or NULL.
	###Adiing the matched peaks and the smimlarity values to spectra.
	
	lret <-vector(length(lspec),mode = "list")
	vsimilarity <- numeric( length( lspec ) )
	vmatched <- vector( mode = "list", length( lspec ) )
	
	if( return.ids.only ){
	    lret <- sapply( lspec, function( x ) {
	    	x$getFieldValue( BIODB.ACCESSION )
	    })
	}else{
	    ###TODO implement three types of return.
	    lret <- lspec
	}
	
	###Reordering the list.
	lret <- lret[ res$ord ]
	

	return( list(measure = res$similarity[ res$ord ], matchedpeaks = res$matched [ res$ord ], id = lret))
})
