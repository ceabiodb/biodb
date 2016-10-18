if ( ! exists('PeakforestConn')) { # Do not load again if already loaded
    
    source('RemoteDbConn.R')
    source('PeakforestEntry.R')
    #source('MsMsSpectrumSearch.R')
    
    #####################
    # CLASS DECLARATION #
    #####################
    
    PeakforestConn <- setRefClass("PeakforestConn", contains = "RemotedbConn") # TODO Inherits also from MassdbConn
    
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

        # TODO Loop on id

        # Request
        jsonstr <- .self$.get.url(get.entry.url(BIODB.PEAKFOREST, id, BIODB.JSON))
        if(jsonstr=="null") return(NULL) # TODO remove: useless

        # Parse JSON
        if ( ! is.na(jsonstr)) { # TODO Test is.null and/or is.na (to check).
            library(RJSONIO)
            jsontree <- fromJSON(jsonstr)

            # TODO Remove (useless) because only one id
            returned.ids <- vapply(jsontree, '[[', FUN.VALUE=NA_real_, i="id")
            
            matched <- match(returned.ids, id)
            content[matched] <- toJSON(jsontree[matched])
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
    
    ##############################
    #PERFORM A NAIVE SEARCH BY MZ#
    ##############################
    
    # TODO Put prototype in MassdbConn
    # TODO ADd option to get either list of entries or data.frame (data frame of spectra only, or data frame of peaks)
    # TODO Remove params param.
    PeakforestConn$methods(searchMz = function(mz, tol, tolunit=BIODB.MZTOLUNIT.VALS, params=list(), drop=TRUE){
        
	                           # TODO match.arg on tolunit
        df <- .make.input.df(mz,tol)
        
        contents <- .self$.do.search.mz(df,BIODB.PEAKFOREST,tolunit,params=list())
        return(contents)
        if(drop&length(contents)==1){
            contents=contents[[1]]
        }
        
        # TODO Return entries instead of JSON. Chemists do not want to read JSON.
        contents
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
