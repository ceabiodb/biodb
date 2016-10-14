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
    
    .extract.from.json.list<-function(jstree,index,sep="/"){
        tindex <- strsplit(index,sep,fixed=TRUE)[[1]]
        if(length(tindex)==1) return(jstree[["index"]])
        value <- jstree
        for(i in 1:length(tindex)){
            while(is.null(names(jstree))){
                if(length(jstree)>1) stop("Multiples fields not supported for JSON")
                jstree=jstree[[1]]
            }
            jstree=jstree[[tindex[i]]]
        }
        jstree
    }
    
    createPeakforestSpectraFromJSON <- function(contents, drop = FALSE) {
        
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
        
        #TO DO peaks
        #jsonfields[[BIODB.NB.PEAKS]] <- 
        for (i in seq_along(contents)){
            
            content <- contents[[i]]
            entry <- PeakForestEntry$new()
            jsontree <- fromJSON(content)
            
            for(field in names(jsonfields)){
                
                tosearch <- jsonfields[[field]]
                value <- .extract.from.json.list(jsontree,tosearch)
                entry$setField(field,value)
                
            }
        
        entries[[i]] <- entry
        
        }
        
    if (drop && length(contents) == 1)
        entries <- entries[[1]]
        
    entries
    }
}#end of the safeguard