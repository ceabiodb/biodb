if ( ! exists('PeakforestConn')) { # Do not load again if already loaded
    
    source('RemoteDbConn.R')
    source('PeakforestEntry.R')
    source('MsMsSpectrumSearch.R')
    
    #####################
    # CLASS DECLARATION #
    #####################
    
    PeakforestConn <- setRefClass("PeakforestConn", contains = "RemotedbConn")
    
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
        jsonstr <- .self$.get.url(get.entry.url(BIODB.PEAKFOREST, id, BIODB.JSON))
        if(jsonstr=="null") return(NULL)
        # Parse JSON
        if ( ! is.na(jsonstr)) {
            library(RJSONIO)
            jsontree <- fromJSON(jsonstr)
            if(length(jsontree)==0) return(NULL)
            #Case where we have a single value.
            if(!is.null(names(jsontree))){
                content[1] <- jsonstr
                return(content)
            }
            returned.ids <- vapply(jsontree, '[[', FUN.VALUE=NA_real_, i="id")
            
            matched <- match(returned.ids, id)
            content[matched] <- toJSON(jsontree[matched])
        }
        return(content)
    })
    
    ##############################
    #PERFORM A NAIVE SEARCH BY MZ#
    ##############################
    
    #Helpers functions.
    
    .make.input.df<-function(mz,tol){
        
        if(length(mz) == 0) return(NULL)
        if(!is.numeric(mz)|!is.numeric(tol)) stop("tol and mz should be numeric.")
        
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
    
    PeakforestConn$methods(.do.search.mz= function(df,db,tolunit,params=list()){
        results <- vector(nrow(df),mode="list")
        for(i in 1:nrow(df)){
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
    
    PeakforestConn$methods(searchMz = function(mz, tol, tolunit=BIODB.MZTOLUNIT.VALS, params=list(), drop=TRUE){
        
        df <- .make.input.df(mz,tol)
        
        contents <- .self$.do.search.mz(df,BIODB.PEAKFOREST,tolunit,params=list())
        return(contents)
        if(drop&length(contents)==1){
            contents=contents[[1]]
        }
        
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
    
    PeakforestConn$methods(peakForestMSMSSearch = function(spec, precursor, mztol, ppm, npmin=2, dmz = 0.001, rtype = c("id")){
        
        rtype <- match.arg(rtype)
        
        rspec <- .self$searchMz(precursor,mztol,BIODB.MZTOLUNIT.PLAIN)
        lspec <- createPeakforestSpectraFromJSON(rspec, drop = TRUE)
        
        rspec <- lapply(lspec,function(x){
            x$getPeaks()[,c(1,2)]
        })
        
        params <- list(ppm = ppm, dmz = dmz)
        
        res <- compareSpectra(spec, rspec, npmin = npmin, fun = MSMS.DIST.WCOSINE, params = params)
        
        if(is.null(res)) return(NULL)
        lret <-vector(length(lspec),mode = "list")
        #print(res$similarity)
        ###Adiing the matched peaks and the smimlarity values to spectra.
        for(i in 1:length(lspec)){
            if(rtype =="id") lret[[i]] <- lspec[[i]]$getFieldValue(BIODB.PEAKFOREST.ID)
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
