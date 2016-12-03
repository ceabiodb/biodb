#####################
# CLASS DECLARATION #
#####################

MassdbConn <- methods::setRefClass("MassdbConn", contains = "BiodbConn")

###############################
# GET CHROMATOGRAPHIC COLUMNS #
###############################

# Get a list of chromatographic columns contained in this database.
# compound.ids  A list of compound IDs used to filter results.
# The returned value is a data.frame with two columns : one for the ID (BIODB.ID) and another one for the title (BIODB.TITLE).
MassdbConn$methods( getChromCol = function(compound.ids = NULL) {
	stop("Method getChromCol() is not implemented in concrete class.")
})

#################
# GET MZ VALUES #
#################

# Returns a numeric vector of all masses stored inside the database.
MassdbConn$methods( getMzValues = function(mode = NULL, max.results = NA_integer_) {
	stop("Method getMzValues() not implemented in concrete class.")
})

################
# GET NB PEAKS #
################

# Returns the number of peaks contained in the database
MassdbConn$methods( getNbPeaks = function(mode = NULL, compound.ids = NULL) {
	stop("Method getNbPeaks() not implemented in concrete class.")
})

#########################
# FIND COMPOUND BY NAME #
#########################

# Find a molecule by name
# name   A vector of molecule names to search for.
# Return an integer vector of the same size as the name input vector, containing the found molecule IDs, in the same order.
MassdbConn$methods( findCompoundByName = function(name) {
	stop("Method findCompoundByName() not implemented in concrete class.")
})

####################################
# FIND SPECTRA IN GIVEN MASS RANGE #
####################################
# Find spectra in the given mass range.
# rtype the type of return, objects, dfspecs data.frame of spectra, dfpeaks data.frame of peaks.
MassdbConn$methods( searchMzRange = function(mzmin, mzmax, rtype = c("objects","dfspecs","dfpeaks")){
	stop("Method searchMzRange() not implemented in concrete class.")
})

####################################
# FIND SPECTRA IN GIVEN MASS RANGE #
####################################
MassdbConn$methods( searchMzTol = function(mz, tol, tolunit=BIODB.MZTOLUNIT.PLAIN, rtype = c("objects","dfspecs","dfpeaks")){
	stop("Method searchMzTol() not implemented in concrete class.")
})

######################################################
# FIND A MOLECULES WITH PRECURSOR WITHIN A TOLERANCE #
######################################################
 MassdbConn$methods( searchSpecPrecTol = function(mz, tol, tolunit=BIODB.MZTOLUNIT.PLAIN, mode = NULL){
	stop("Method searchSpecPrecTol not implemented in concrete class.")
 })

#################################
#perform a database MS-MS search#
#################################

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
	lspec <- .self$searchSpecPrecTol( precursor, mztol, BIODB.MZTOLUNIT.PLAIN, mode = mode)
	rspec <- lapply(lspec,function(x){
        peaks <- x$getFieldValue(BIODB.PEAKS)
		
		####Getting the correct fields
		vcomp <- c(BIODB.PEAK.MZ, BIODB.PEAK.RELATIVE.INTENSITY, BIODB.PEAK.INTENSITY)
		
		foundfields <- vcomp %in% colnames(peaks)
		if(sum(foundfields ) < 2){
			stop(paste0("fields can't be coerced to mz and intensity : ",colnames(peaks)))
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
