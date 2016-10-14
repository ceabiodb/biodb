if ( ! exists('PeakforestConn')) { # Do not load again if already loaded
    
    source('RemoteDbConn.R')
    source('PeakforestEntry.R')
    
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
    
    ################
    # CREATE ENTRY #
    ################
    
    # Creates a Spectrum instance from file content.
    # content       A file content, downloaded from the public database.
    # RETURN        A spectrum instance.
    PeakforestConn$methods( createEntry = function(content, drop = TRUE) {
        return(createPeakforestSpectrumFromJson(content, drop = drop))
    })
}
