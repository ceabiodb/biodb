if ( ! exists('PeakForestSpectrumEntry')) { # Do not load again if already loaded
	
	if(!require(RJSONIO)) stop("RJSONIO required")
	source('BiodbEntry.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	# TODO Create class PeakforestCompoundEntry
	PeakForestSpectrumEntry <- setRefClass("PeakForestSpectrumEntry", contains = "BiodbEntry")
	
	PeakForestCompoundEntry <- setRefClass("PeakForestCompoundEntry", contains = "BiodbEntry")
	
	
	###########
	# FACTORY #
	###########
	
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
	
	
	###Arg is jcontent ot indicate that the content is already a json.
	createPeakforestCompoundFromJSON(contents, drop = FALSE){
		
		if(is.character(contents)) contents <- fromJSON(contents)
		
		jsonfields <- list()
		jsonfields[[ACCESSION]] <- "id"
		jsonfields[[BIODB.PUBCHEMCOMP.ID]] <- "PubChemCID"
		jsonfields[[BIODB.CHEBI.ID]] <- "ChEBI"
		jsonfields[[BIODB.HMDB.ID]] <- "HMDB"
		jsonfields[[BIODB.KEGG.ID]] <- "KEGG"
		jsonfields[[BIODB.FORMULA]] <- "formula"
		jsonfields[[BIODB.SMILES]] <- "canSmiles"
		jsonfields[[BIODB.AVERAGE.MASS]] <- "averageMass"
		jsonfields[[BIODB.MONOISOTOPIC.MASS]] <- "monoisotopicMass"
		jsonfields[[BIODB.INCHI]] <- "inChI"
		jsonfields[[BIODB.INCHIKEY]] <- "inchiIKey"
		jsonfields[[BIODB.NAME]] <- "mainName"

		entries <- vector(length(contents),mode="list")
		
		for (i in seq_along(contents)){
			
			content <- contents[[i]]
			entry <- PeakForestCompoundEntry$new()
			
			
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
			jsontree <- fromJSON(content)
			cnames <- c(BIODB.PEAK.MZ, BIODB.PEAK.RELATIVE.INTENSITY, BIODB.PEAK.FORMULA, BIODB.PEAK.MZTHEO, BIODB.PEAK.ERROR.PPM)
			
			entry <- PeakForestSpectrumEntry$new()
			
			
			for(field in names(jsonfields)){
				
				tosearch <- jsonfields[[field]]
				value <- .extract.from.json.list(jsontree,tosearch)
				entry$setField(field,value)
			}
			
			######################
			# TREATING THE PEAKS #
			######################
			
			entry$setField(BIODB.NB.COMPOUNDS,length(jsontree$peaks))
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
			
			##################################
			# TREATING THE LIST OF COMPOUNDS #
			##################################
			
			entry$setField(BIODB.NB.COMPOUNDS,length(jsontree$listOfCompounds))
			compounds <- list
			
			###Parsing compounds.
			if( length( jsontree$listOfCompounds) != 0){
				compounds <- lapply( jsontree$listOfCompounds, function(x){
					createPeakforestCompoundFromJSON(x)
				})
			}
			
			entry$setField(BIODB.COMPOUNDS, compounds)
			
			
			entries[[i]] <- entry
		}
		
		
		if (drop && length(contents) == 1)
			entries <- entries[[1]]
		
		entries
	}
	
	
	
}#end of the safeguard
