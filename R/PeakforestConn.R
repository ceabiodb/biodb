#####################
# CLASS DECLARATION #
#####################
#'A class to connect to peakforest
#'@export
#'@field .url An urel to the database
PeakforestConn <- methods::setRefClass("PeakforestConn", contains = c("RemotedbConn","MassdbConn"), fields = list( .url = "character" )) # TODO Inherits also from MassdbConn

##########################
# GET ENTRY CONTENT TYPE #
##########################

PeakforestConn$methods( getEntryContentType = function(type) {
	return(BIODB.JSON) 
})

#####################
# GET ENTRY CONTENT #
#####################

PeakforestConn$methods( getEntryContent = function(id) {
	
	
	# Initialize return values
	content <- rep(NA_character_, length(id))
	# Request
	
	url <- get.entry.url(BIODB.PEAKFOREST, id[i], BIODB.JSON,token = .self$.token)
	jsonstr <- .self$.get.url(url)
	if(startsWith("<html>", jsonstr) ){
		next
	}
	
	return(content)
})


##########################################
# SEARCH FOR SPECTRA IN GIVEN MASS RANGE #
##########################################

PeakforestConn$methods( searchMzRange = function(mzmin, mzmax, rtype = c("object","spec","peak")){
	
	rtype <- match.arg(rtype)
	if(mzmin>mzmax){
		stop("mzmin shloud be inferior to mzmax in searchMzRange.")
	}
	
	url <- paste0("https://rest.peakforest.org/spectra/lcms/peaks/get-range/",mzmin,"/",mzmax)
	
	contents <-  .self$.get.url(url)
	
	jsontree <- fromJSON(contents)
	
	###No match form the output.
	if( length(jsontree)==0 ) return(NULL)
	
	# Getting a list of all the id.
	lid <- sapply(jsontree,function(x){
		x$source$id
	})
	
	# Returning the content for all the spectra
	contents <- .self$getEntryContent(lid)
	
	entries  <- .self$createEntry(contents)
	
	# Checking the return type
	if( rtype=="object" ){
		return( entries )
	}
	
	### XXXX See if we don't want to reduce the output and factorize this shit.
	toreturn <- NULL
	if( rtype=="spec" ){
		toreturn <- sapply(entries,function(x){
			x$getFieldsAsDataFrame()
		})
	}
	if( rtype=="peak" ){
		toreturn <- lapply(entries,function(x){
			temp <- as.data.frame( x$getFieldValue( BIODB.PEAKS ))
			temp$accession = x$getFieldValue( BIODB.ACCESSION) 
			return(temp)
			
		})
	}
	###Trying to convert in data.frame
	if(!is.data.frame(toreturn)){
		temp <- colnames(toreturn[[1]])
		toreturn <- do.call("rbind.fill",toreturn)
		colnames(toreturn) <- temp
	}
	
	return(toreturn)
})


#################################################
# SEARCH FOR SPECTRA IN A TOLERANCE AROUND A MZ #
#################################################

PeakforestConn$methods( searchMzTol = function(mz, tol, tolunit=BIODB.MZTOLUNIT.VALS,
											   rtype = c("object","spec","peak")){
	
	rtype <- match.arg(rtype)
	tolunit <- match.arg(tolunit)
	
	if( tolunit == BIODB.MZTOLUNIT.PPM){
		tol <- tol * mz * 10^-6
	}
	
	mzmin <- mz - tol
	mzmax <- mz + tol
	
	return(.self$searchMzRange(mzmin, mzmax, rtype = rtype))
	
})

##################################################
# SEARCH FOR MSMS SPECTRA PRECUSOR AROUND A MASS #
##################################################


PeakforestConn$methods(
	searchSpecPrecTol = function(mz,
								 tol,
								 tolunit = "plain",
								 mode = NULL) {
		#TODO handle the units
		#tolunit <- match.arg(tolunit)
		
		strmode <- ''
		
		if (!is.null(mode)) {
			if (mode %in% c(BIODB.MSMODE.NEG, BIODB.MSMODE.POS)) {
				strmode <- paste0('?polarity=', mode)
			}
			
		}
		
		if (tolunit == BIODB.MZTOLUNIT.PPM) {
			tol <- tol * mz * 10 ^ -6
		}
		
		##Request which return peak and not spectra.
		url <-
			paste0(
				"https://rest.peakforest.org/spectra/lcms/search-naive/",
				mz,
				"/",
				tol,
				strmode
			)
		contents <-  .self$.get.url(url)
		entries  <- .self$createReducedEntry(contents, drop = TRUE)
		return(entries)
	}
)


################
# CREATE ENTRY #
################

# Creates a Spectrum instance from file content.
# content       A file content, downloaded from the public database.
# RETURN        A spectrum instance.
PeakforestConn$methods( createEntry = function(content, drop = TRUE) {
	return(createPeakforestSpectraFromJSON(content, drop = drop))
})

PeakforestConn$methods( createReducedEntry = function(content , drop = TRUE){
	entries <- createReducedSpectraFromJSON(content, drop = drop)	
	return(entries)
})
