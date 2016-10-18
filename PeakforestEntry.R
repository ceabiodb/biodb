if ( ! exists('PeakForestSpectrumEntry')) { # Do not load again if already loaded
	
	if(!require(RJSONIO)) stop("RJSONIO required")
	source('BiodbEntry.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	# TODO Create class PeakforestCompoundEntry
	PeakForestSpectrumEntry <- setRefClass("PeakForestSpectrumEntry", contains = "BiodbEntry")
	
	
	###########
	# FACTORY #
	###########
	
	# TODO Useless -> remove it and replace by a getFields
	PeakForestSpectrumEntry$methods( getPeaks = function(){
		as.data.frame(.self$getFieldValue(BIODB.PEAKS))
	})
	
	# TODO To remove.
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
		jsonfields[[BIODB.ACCESSION]] <- "id" # TODO Use BIODB.ACCESSION instead
		jsonfields[[BIODB.MSMODE]] <- "polarity"
		jsonfields[[BIODB.MSDEV]] <- "analyzerMassSpectrometerDevice/instrumentName"
		jsonfields[[BIODB.MSDEVTYPE]] <- "analyzerMassSpectrometerDevice/ionAnalyzerType"
		
		# TODO verify if it sub or comp
		# TODO Since a spectrum can be associated with several compounds and listOfCompounds contain all compound info, we will create a field BIODB.COMPOUND of type BiodbCompoundEntry with cardinality MANY
		# TODO Do a loop on all elements of listOfCompounds and for each subJSON call createPeakforestCompoundFromJSON
		jsonfields[[BIODB.PUBCHEMCOMP.ID]] <- "PubChemCID"
		jsonfields[[BIODB.CHEBI.ID]] <- "listOfCompounds/ChEBI"
		jsonfields[[BIODB.HMDB.ID]] <- "listOfCompounds/HMDB"
		jsonfields[[BIODB.KEGG.ID]] <- "listOfCompounds/KEGG"
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
			
			entry <- PeakForestSpectrumEntry$new()
			
			
			for(field in names(jsonfields)){
				
				tosearch <- jsonfields[[field]]
				value <- .extract.from.json.list(jsontree,tosearch)
				entry$setField(field,value)
			}
			
			entry$setField(BIODB.NB.PEAKS,length(jsontree$peaks))
			peaks <- data.frame( matrix( 0,ncol = length(cnames), nrow = 0))
			colnames(peaks) <- cnames
			###Parsing peaks.
			if(length(jsontree$peaks) != 0){
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
				},FUN.VALUE = NA_character_)
				
				peaks<-t(peaks)
				colnames(peaks)<-cnames
			}
			
			entry$setField(BIODB.PEAKS,peaks)
			
			entries[[i]] <- entry
		}
		
		
		if (drop && length(contents) == 1)
			entries <- entries[[1]]
		
		entries
	}
}#end of the safeguard
