#####################
# CLASS DECLARATION #
#####################

# TODO Create class PeakforestCompoundEntry
PeakForestSpectrumEntry <- methods::setRefClass("PeakForestSpectrumEntry", contains = "BiodbEntry")

PeakForestCompoundEntry <- methods::setRefClass("PeakForestCompoundEntry", contains = "BiodbEntry")


###########
# FACTORY #
###########


###Arg is jcontent ot indicate that the content is already a json.
createPeakforestCompoundFromJSON <- function(contents, drop = FALSE) {
	
	if(is.character(contents))
		contents <- jsonlite::fromJSON(contents, simplifyDataFrame=FALSE)
	
	jsonfields <- list()
	jsonfields[[BIODB.ACCESSION]] <- "id"
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
		
		jsontree <- contents[[i]]
		entry <- PeakForestCompoundEntry$new()
		
		
		for(field in names(jsonfields)){
			
			tosearch <- jsonfields[[field]]
			value <- jsontree$tosearch
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
	
	
	###Checking that it's a list.
	if(length(contents) == 1){
		if(startsWith(contents[[1]], "<html>") ){
			return(NULL)
		}else{
			contents <- jsonlite::fromJSON(contents[[1]],simplifyDataFrame=FALSE)	
			
		}
	}
	
	for (i in seq_along(contents)){
		
		content <- contents[[i]]
		jsontree <- NULL
		if(typeof(content) == "character"){
			if(startsWith(content, "<html>")|content=="null"){
				entries[[i]] <- NULL
				next
			}
			jsontree <- jsonlite::fromJSON(content,simplifyDataFrame=FALSE)
		}else{
			jsontree <- content
		}
		cnames <- c(BIODB.PEAK.MZ, BIODB.PEAK.RELATIVE.INTENSITY, BIODB.PEAK.FORMULA, BIODB.PEAK.MZTHEO, BIODB.PEAK.ERROR.PPM)
		
		entry <- PeakForestSpectrumEntry$new()
		#####Setting thz mass analyzer
		entry$setField(BIODB.MSDEV,jsontree$analyzerMassSpectrometerDevice$instrumentName)
		entry$setField(BIODB.MSDEVTYPE,jsontree$analyzerMassSpectrometerDevice$ionAnalyzerType)	
		
		
		
		for(field in names(jsonfields)){
			
			tosearch <- jsonfields[[field]]
			value <- jsontree$tosearch
			entry$setField(field,value)
		}
		
		######################
		# TREATING THE PEAKS #
		######################
		
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
		
		##################################
		# TREATING THE LIST OF COMPOUNDS #
		##################################
		
		entry$setField(BIODB.NB.COMPOUNDS,length(jsontree$listOfCompounds))
		compounds <- list()
		
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


####TDO CLEAN THIS

createReducedSpectraFromJSON <- function(contents,
			 drop = FALSE,
			 checkSub = TRUE) {
	entries <- vector(length(contents), mode = "list")
	jsonfields <- character()
	# jsonfields[[BIODB.ACCESSION]] <-
	# 	"id" # TODO Use BIODB.ACCESSION instead
	
	
	###Checking that it's a list.
	if (length(contents) == 1) {
		if (startsWith(contents[[1]], "<html>")) {
			return(NULL)
		} else{
			contents <- jsonlite::fromJSON(contents[[1]], simplifyDataFrame=FALSE)
			
		}
	}
	
	for (i in seq_along(contents)) {
		content <- contents[[i]]
		jsontree <- NULL
		if (typeof(content) == "character") {
			if (startsWith(content, "<html>") | content == "null") {
				entries[[i]] <- NULL
				next
			}
			jsontree <- jsonlite::fromJSON(content, simplifyDataFrame=FALSE)
		} else{
			jsontree <- content
		}
		
		
		cnames <-
			c(
				BIODB.PEAK.MZ,
				BIODB.PEAK.RELATIVE.INTENSITY,
				BIODB.PEAK.FORMULA,
				BIODB.PEAK.MZTHEO,
				BIODB.PEAK.ERROR.PPM
			)
		
		entry <- PeakForestSpectrumEntry$new()
		entry$setField(BIODB.ACCESSION, jsontree$id)
		
		######################
		# TREATING THE PEAKS #
		######################
		
		entry$setField(BIODB.NB.PEAKS, length(jsontree$peaks))
		peaks <- data.frame(matrix(0, ncol = length(cnames), nrow = 0))
		colnames(peaks) <- cnames
		###Parsing peaks.
		if (length(jsontree$peaks) != 0) {
			peaks <- sapply(jsontree$peaks, function(x) {
				return(
					list(
						as.double(x$mz),
						as.integer(x$ri),
						as.character(x$composition),
						as.double(x$theoricalMass),
						as.double(x$deltaPPM)
					)
				)
			})
			###Removing all whitespaces from the formule.
			peaks[3, ] <- vapply(peaks[3, ], function(x) {
				gsub(" ", "", trimws(x))
			}, FUN.VALUE = NA_character_)
			
			peaks <- as.data.frame(t(peaks))
			colnames(peaks) <- cnames
		}
		
		entry$setField(BIODB.PEAKS, peaks)
		
		entries[[i]] <- entry
	}
	
	
	if (drop && length(contents) == 1)
		entries <- entries[[1]]
	
	entries
}
