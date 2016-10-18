if ( ! exists('PeakforestConn')) { # Do not load again if already loaded
	
	source('RemoteDbConn.R')
	source('PeakforestEntry.R')
	#source('MsMsSpectrumSearch.R')
	
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
			jsonstr <- .self$.get.url(get.entry.url(BIODB.PEAKFOREST, id, BIODB.JSON))
			
			if(startsWith("<html>", jsonstr) ){
				next
			}
			
			content[i] <- jsonstr
			
		}
		
		return(content)
	})
	
	################################
	# PERFORM A NAIVE SEARCH BY MZ #
	################################
	
	# Helpers functions.
	
	# TODO What is not used now, must be removed.
	.make.input.df<-function(mz, tol) {
		
		if (length(mz) == 0) return(NULL)
		if ( ! is.numeric(mz) | ! is.numeric(tol)) stop("tol and mz should be numeric.")
		
		df <- NULL
		tol <- 
			if(length(mz)==length(tol)){
				df <- data.frame(matrix(c(mz,tol),byrow=FALSE,ncol=2))
				colnames(df) <- c(BIODB.PEAK.MZ, BIODB.TOL)
			}
		if(length(mz)>1 & length(tol)==1){
			df <- data.frame(matrix(c(mz,rep(tol,length(mz))),byrow=FALSE,ncol=2))
			colnames(df) <- c(BIODB.PEAK.MZ, BIODB.TOL)
		}
		
		df
	}
	
	# TODO move this code into searchMz
	PeakforestConn$methods(.do.search.mz= function(df,db,tolunit,params=list()){
		results <- vector(nrow(df),mode="list")
		for(i in 1:nrow(df)){
			# TODO make directly the URL, no function call.
			# TODO Use "'spectra/lcms/peaks/get-range/', mz.low, '/', mz.high" instead.
			allurl <- .self$.get.url(get.mass.search.url(class = db,
														 mass = df[i,BIODB.PEAK.MZ],
														 tol=df[i,BIODB.TOL],
														 tolunit = tolunit,
														 supp = params,content.type = BIODB.JSON))
			results[[i]] <- allurl
		}
		results
	})
	
	################################
	# SEARCH FOR SPECTRA IN GIVEN RANGE #
	################################
	
	# TODO ADd option to get either list of entries or data.frame (data frame of spectra only, or data frame of peaks)
	# TODO Remove params param.
	PeakforestConn$methods( searchMzRange = function(mzmin, mzmax, rtype = c("object","spec","peak")){
		
		rtype <- match.arg(rtype)
		if(mzmin>mzmax){
			stop("mzmin shloud be inferior to mzmax in searchMzRange.")
		}
		
		df <- .make.input.df(mz,tol)
		url <- paste0("https://rest.peakforest.org/spectra/lcms/peaks/get-range/",mzmin,"/",mzmax)
		
		contents <-  .self$.get.url(url)
		
		# TODO When webservice is fixed remove the peak list.
		library(RJSONIO)
		
		jsontree <- fromJSON(contents)
		
		###No match form the output.
		if( length(jsontree)==0 ) return(NULL)
		
		# Getting a list of all the id.
		lid <- sapply(jsontree){function(x){
			x$source$id
		}}
		
		# Returning the content for all the spectra
		contents <- .self$getEntryContent(lid)
		
		entries  <- .self$createEntry(contents)
		
		# Checking the return type
		if( rtype=="object" ){
			return( entries )
		}
		
		### XXXX See if we don't want to reduce the output.
		toreturn <- NULL
		if( rtype=="spec" ){
			toreturn <- sapply(entries,function(x){
				.x$getFieldsAsDataFrame()
			})
		}
		if( rtype=="peak" ){
			toreturn <- sapply(entries,function(x){
				.x$getFieldValue( BIODB.PEAKS )
			})
		}
		
		return(toreturn)
	})
	
	
	
	
	###TO DO a function to handle the supplementary arguments correctly.
	.do.get.search.url <- function(class, mass, tol, tolunit, supp = NULL, content.type = BIODB.HTML, base.url = NA_character_, token = NA_character_) {
		if ( ! class %in% c(BIODB.PEAKFOREST)){
			stop(paste0("Class ", class, " not implemented yet."))
		}
		tol <- .tol.mass(tol,mass,type=tolunit)
		
		# Only certain databases can handle multiple searchs
		if ( ! class %in% c(BIODB.PEAKFOREST) && length(mass) > 1)
			stop(paste0("Cannot build a URL for performing multiple searches for class ", class, "."))
		#https://rest.peakforest.org/spectra/lcms/search-naive/205.097,188.0703/0.25?matchAll=false&
		
		####Put this part in a separate function
		strsupp <- ''
		if( !is.null(supp)&length(supp)>0){
			strsupp <- switch(class,
							  peakforest=paste(paste(names(supp),unlist(supp),sep = '='),collapse = ",")
			)
		}
		####
		
		
		# Get URL
		url <- switch(class,
					  peakforest  = switch(content.type,
					  					 json= paste0('https://rest.peakforest.org/spectra/lcms/search-naive/',
					  					 			 paste(mass,collapse=","),"/",tol,strsupp)),
					  
					  NULL
		)
		return(url)
	}
	
	
	
	get.mass.search.url <- function(class, mass, tol, tolunit, supp = NULL, content.type = BIODB.HTML, max.length = 0, base.url = NA_character_, token = NA_character_) {
		
		# TODO Use "'spectra/lcms/peaks/get-range/', mz.low, '/', mz.high" instead.
		
		if (length(mass) == 0)
			return(NULL)
		
		full.url <- .do.get.search.url(class, mass, tol, tolunit, supp, content.type = content.type, base.url = base.url, token = token)
		if (max.length == 0 || nchar(full.url) <= max.length)
			return(if (max.length == 0) full.url else list(url = full.url, n = length(mass)))
		
		# Find max size URL
		a <- 1
		b <- length(mass)
		while (a < b) {
			m <- as.integer((a + b) / 2)
			url <- .do.get.search.url(class, mass[1:m], content.type = content.type, base.url = base.url, token = token)
			if (nchar(url) <= max.length && m != a)
				a <- m
			else
				b <- m
		}
		url <- .do.get.search.url(class, mass[1:a], content.type = content.type, base.url = base.url, token = token)
		
		return(list( url = url, n = a))
	}
	
	
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
