if ( ! exists('BIODB.MZTOLUNIT.PPM')) {
    
    source("biodb-common.R")
    source(file.path('..', 'r-lib', 'UrlRequestScheduler.R'), chdir = TRUE)
    
    # TODO remove this file, and move its content to either biodb-common.R or PeakforestConn.R

    # Tolerance units
    BIODB.TOL <- 'mztol'
    BIODB.MZTOLUNIT.PPM <- 'ppm'
    BIODB.MZTOLUNIT.PLAIN <- 'plain' # same as mz: mass-to-charge ratio
    BIODB.MZTOLUNIT.VALS <- c(BIODB.MZTOLUNIT.PPM, BIODB.MZTOLUNIT.PLAIN)
    
    # TODO Rename as .convert.ppm.tol.to.plain.tol
    # TODO Or instead right a function .get.mz.range(mass, tol, type = )
    .tol.mass<-function(value,mass,type=BIODB.MZTOLUNIT.VALS){
        type=match.arg(type)
        if(type=="ppm"){
            return(value*10^-6*mass)
        }
        return(value)
        
    }
    
    ###tol the value of the tolerance, tol unit the unit of the tolerance, supp, the suuplementary option
    ###to be passed to the database.
    
    ###TO DO a function to handle the supplementary arguments correctly.
    .do.get.search.url <- function(class, mass, tol, tolunit, supp = NULL, content.type = BIODB.HTML, base.url = NA_character_, token = NA_character_) {
        if ( ! class %in% c(BIODB.PEAKFOREST)){
            stop(paste0("Class ", class, " not implemented yet."))
        }
        tol <- .tol.mass(tol,mass,type=tolunit)
        
        # Only certain databases can handle multiple searchs
        if ( ! class %in% c(BIODB.PEAKFOREST) && length(mass) > 1)
            stop(paste0("Cannot build a URL for getting multiple entries for class ", class, "."))
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
    
    
} # end of safeguard.
