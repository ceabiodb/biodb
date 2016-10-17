if ( ! exists('PeakForestEntry')) { # Do not load again if already loaded
    
    if(!require(RJSONIO)) stop("RJSONIO required")
    source('BiodbEntry.R')
    
    #####################
    # CLASS DECLARATION #
    #####################
    
    PeakForestEntry <- setRefClass("PeakForestEntry", contains = "BiodbEntry")
    
    
    ###########
    # FACTORY #
    ###########
    
    PeakForestEntry$methods( getPeaks = function(){
        as.data.frame(.self$getFieldValue(BIODB.PEAKS))
    })
    
    .extract.from.json.list<-function(jstree,index,sep="/"){
        tindex <- strsplit(index,sep,fixed=TRUE)[[1]]
        if(length(tindex)==1) return(jstree[[index]])
        for(i in 1:length(tindex)){
            if(is.null(names(jstree))&length(jstree)>=1){
                
                return(sapply(jstree,'[[',i=tindex[i]))
            }
            jstree=jstree[[tindex[i]]]
        }
        jstree
    }
    
    createPeakforestSpectraFromJSON <- function(contents, drop = FALSE, checkSub = TRUE) {
        
        entries <- vector(length(contents),mode="list")
        jsonfields <- character()
        jsonfields[[BIODB.PEAKFOREST.ID]] <- "id"
        jsonfields[[BIODB.MSMODE]] <- "polarity"
        jsonfields[[BIODB.MSDEV]] <- "analyzerMassSpectrometerDevice/instrumentName"
        jsonfields[[BIODB.MSDEVTYPE]] <- "analyzerMassSpectrometerDevice/ionAnalyzerType"
        
        jsonfields[[BIODB.CHEBI.ID]] <- "listOfCompounds/ChEBI"
        jsonfields[[BIODB.HMDB.ID]] <- "listOfCompounds/HMDB"
        jsonfields[[BIODB.KEGG.ID]] <- "listOfCompounds/KEGG"
        ###TO DO verify if it sub or comp
        jsonfields[[BIODB.PUBCHEMCOMP.ID]] <- "PubChemCID"
        jsonfields[[BIODB.FORMULA]] <- "listOfCompounds/formula"
        jsonfields[[BIODB.SMILES]] <- "listOfCompounds/canSmiles"
        jsonfields[[BIODB.AVERAGE.MASS]] <- "listOfCompounds/averageMass"
        jsonfields[[BIODB.MONOISOTOPIC.MASS]] <- "listOfCompounds/monoisotopicMass"
        jsonfields[[BIODB.INCHI]] <- "listOfCompounds/inChI"
        jsonfields[[BIODB.INCHIKEY]] <- "listOfCompounds/inchiIKey"
        
        
        for (i in seq_along(contents)){
            
            content <- contents[[i]]
            ###Going on level down once again if needed.
            
            entry <- NULL
            jsontree <- fromJSON(content)
            cnames <- c(BIODB.PEAK.MZ, BIODB.PEAK.RELATIVE.INTENSITY, BIODB.PEAK.FORMULA, BIODB.PEAK.MZTHEO, BIODB.PEAK.ERROR.PPM)
            
            
            
            if(is.null(names(jsontree))){  ###Case of multiples spectra in the json.
                
                entries[[i]]<-list()
                
                for(j in seq_along(jsontree)){
                    
                    entry <- PeakForestEntry$new()
                    
                    for(field in names(jsonfields)){
                        
                        tosearch <- jsonfields[[field]]
                        value <- .extract.from.json.list(jsontree[[j]],tosearch)
                        entry$setField(field,value)
                    }
                    
                    entry$setField(BIODB.NB.PEAKS,length(jsontree[[j]]$peaks))
                    
                    ###Parsing peaks.
                    peaks <- sapply(jsontree[[j]]$peaks,function(x){
                        return(list(as.double(x$mz),
                                    as.double(x$ri),
                                    as.character(x$composition),
                                    as.double(x$theoricalMass),
                                    as.double(x$deltaPPM)
                        ))
                    })
                    
                    peaks<-t(peaks)
                    colnames(peaks)<-cnames
                    entry$setField(BIODB.PEAKS,peaks)
                    
                    entries[[i]][[j]] <- entry
                }
            }else{
                entry <- PeakForestEntry$new()
                
                
                for(field in names(jsonfields)){
                    
                    tosearch <- jsonfields[[field]]
                    value <- .extract.from.json.list(jsontree,tosearch)
                    entry$setField(field,value)
                }
                
                entry$setField(BIODB.NB.PEAKS,length(jsontree$peaks))
                
                ###Parsing peaks.
                peaks <- sapply(jsontree$peaks,function(x){
                    return(list(as.double(x$mz),
                                as.integer(x$ri),
                                as.character(x$composition),
                                as.double(x$theoricalMass),
                                as.double(x$deltaPPM)
                    ))
                })
                
                ###Removing all whitespaces from the formule.
                peaks[3,]<-vapply(peaks[3,],function(x){
                    gsub(" ","",trimws(x))
                },FUN.VALUE = NULL)
                
                peaks<-t(peaks)
                colnames(peaks)<-cnames
                entry$setField(BIODB.PEAKS,peaks)
                
                entries[[i]] <- entry
            }
        }
        
        if (drop && length(contents) == 1)
            entries <- entries[[1]]
        
        entries
    }
}#end of the safeguard