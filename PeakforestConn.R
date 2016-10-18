if ( ! exists('PeakforestConn')) { # Do not load again if already loaded
	
	source('RemoteDbConn.R')
	source('PeakforestEntry.R')
	source('MassdbConn.R')
	library(plyr)
	
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	PeakforestConn <- setRefClass("PeakforestConn", contains = c("RemotedbConn","MassdbConn"), fields = list( .url = "character" )) # TODO Inherits also from MassdbConn
	
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
		
		for(i in 1:length(id)){
			
			# Request
			url <- get.entry.url(BIODB.PEAKFOREST, id[i], BIODB.JSON)
			jsonstr <- .self$.get.url(url)
			if(startsWith("<html>", jsonstr) ){
				next
			}
			
			content[i] <- jsonstr
			
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
		
		# TODO When webservice is fixed remove the peak list.
		library(RJSONIO)
		
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
												   rtype = c("objects","dfspecs","dfpeaks")){
		
		rtype <- match.arg(rtype)
		tolunit <- match.arg(tolunit)
		
		if( tolunit == BIODB.MZTOLUNIT.PPM){
			tol <- tol * mz * 10^-6
		}
		
		mzmin <- mz - tol
		mzmax <- mz + tol
		
		return(.self$searchMzRange(mzmin, mzmax, rtype = rtype))
		
	})
	
	
	#################################
	#perform a database MS-MS search#
	#################################
	
	###spec : the spec to match against the database.
	###precursor : the mass/charge of the precursor to be looked for.
	###mtol : the size of the windows arounf the precursor to be looked for.
	###ppm : the matching ppm tolerance.
	###dmz : the mass tolerance is taken as the minium between this quantity and the ppm.
	###npmin : the minimum number of peak to detect a match (2 recommended)
	
	# Put this method into MassdbConn and rename it msmsSearch
	# TODO Rename rtype in return.ids.only and make it a boolean
	# TODO Returns list( msmssim = vector(), matchedpeaks = list(), spectra = either list of entries or data.frame)
	PeakforestConn$methods(peakForestMSMSSearch = function(spec, precursor, mztol, ppm, npmin=2, dmz = 0.001, rtype = c("id")){
		
		rtype <- match.arg(rtype)
		
		rspec <- .self$searchMz(precursor,mztol,BIODB.MZTOLUNIT.PLAIN)
		# TODO No. We want searchMz to return the list of entries
		lspec <- createPeakforestSpectraFromJSON(rspec, drop = TRUE)
		
		rspec <- lapply(lspec,function(x){
			# TODO Test that the peak data frame contains the column BIODB.PEAK.MZ and either BIODB.PEAK.INTENSITY or BIODB.PEAK.RELATIVE.INTENSITY
			x$getPeaks()[,c(1,2)] # Use peak data frame column names from biodb-common
		})
		
		params <- list(ppm = ppm, dmz = dmz)
		
		# TODO Import compareSpectra into biodb and put it inside massdb-helper.R or hide it as a private method.
		# TODO Add parameters fun and params to msmsSearch
		res <- compareSpectra(spec, rspec, npmin = npmin, fun = MSMS.DIST.WCOSINE, params = params)
		
		if(is.null(res)) return(NULL) # To decide at MassdbConn level: return empty list (or empty data frame) or NULL.
		lret <-vector(length(lspec),mode = "list")
		#print(res$similarity)
		###Adiing the matched peaks and the smimlarity values to spectra.
		
		for(i in 1:length(lspec)){
			if(rtype =="id") lret[[i]] <- lspec[[i]]$getFieldValue(BIODB.PEAKFOREST.ID) # Change into BIODB.ACCESSION
			lret[[i]]$MsMsSim <- res$similarity[i]
			lret[[i]]$matchedPeaks <- res$matched[[i]]
		}
		###Reordering the list.
		return(lret[res$ord])
	})
	
	
	
	
	
	################
	# CREATE ENTRY #
	################
	
	# Creates a Spectrum instance from file content.
	# content       A file content, downloaded from the public database.
	# RETURN        A spectrum instance.
	PeakforestConn$methods( createEntry = function(content, drop = TRUE) {
		return(createPeakforestSpectraFromJSON(content, drop = drop))
	})
}
