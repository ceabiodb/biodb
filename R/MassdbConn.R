# vi: fdm=marker

#' @include BiodbConn.R

# Class declaration {{{1
################################################################

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
MassdbConn$methods( getMzValues = function(mode = NULL, max.results = NA_integer_) {
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
MassdbConn$methods( findCompoundByName = function(name) {
	.self$.abstract.method()
})

# Search peak {{{1
################################################################

MassdbConn$methods( searchPeak = function(mz = NA_real_, tol = NA_real_, relint = NA_integer_, mode = NA_character_, max.results = NA_integer_) {
	"Search matching peaks in database, and return a list of entry IDs."

	if (is.na(mz) || length(mz) == 0)
		.self$message(MSG.ERROR, "At least one mz value is required for searchPeak() method.")
	if (! all(mz > 0))
		.self$message(MSG.ERROR, "MZ values must be positive for searchPeak() method.")
	if (is.na(tol) || length(tol) == 0)
		.self$message(MSG.ERROR, "A tolerance value is required for searchPeak() method.")
	if (length(tol) > 1)
		.self$message(MSG.ERROR, "No more than one tolerance value is allowed for searchPeak() method.")
	if ( ! is.na(max.results) && max.results < 0)
		.self$message(MSG.ERROR, "The maximum number of results must be positive or zero for searchPeak() method.")
	if ( ! is.na(mode) && ! mode %in% c(BIODB.MSMODE.NEG, BIODB.MSMODE.POS))
		.self$message(MSG.ERROR, paste("Incorrect MS mode", mode, "for searchPeak() method."))
	if ( ! is.na(relint) && relint < 0)
		.self$message(MSG.ERROR, "The relative intensity must be positive or zero for searchPeak() method.")

	.self$.do.search.peak(mz = mz, tol = tol, relint = relint, mode = mode, max.results = max.results)
})

# Do search peak {{{1
################################################################

MassdbConn$methods( .do.search.peak = function(mz = NA_real_, tol = NA_real_, relint = NA_integer_, mode = NA_character_, max.results = NA_integer_) {
	.self$.abstract.method()
})

# Find spectra in given mass range {{{1
################################################################

# Find spectra in the given mass range.
# rtype the type of return, objects, dfspecs data.frame of spectra, dfpeaks data.frame of peaks.
MassdbConn$methods( searchMzRange = function(mzmin, mzmax, rtype = c("objects","dfspecs","dfpeaks")){
	.self$.abstract.method()
})

# Find spectra in given mass range {{{1
################################################################

MassdbConn$methods( searchMzTol = function(mz, tol, tolunit=BIODB.MZTOLUNIT.PLAIN, rtype = c("objects","dfspecs","dfpeaks")){
	.self$.abstract.method()
})

# Find molecules with precursor within a tolerance {{{1
################################################################

MassdbConn$methods( searchSpecPrecTol = function(mz, tol, tolunit=BIODB.MZTOLUNIT.PLAIN, mode = NULL){
	.self$.abstract.method()
 })

# Perform a database MS-MS search {{{1
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
	lspec <- .self$searchSpecPrecTol( precursor, mztol, BIODB.MZTOLUNIT.PLAIN, mode = mode)
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
