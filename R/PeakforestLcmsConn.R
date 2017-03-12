# vi: fdm=marker

# Class declaration {{{1
################################################################

#'PeakForest connector class.
#'@export
PeakforestLcmsConn <- methods::setRefClass("PeakforestLcmsConn", contains = c("PeakforestConn", "MassdbConn"))

# Get entry content url {{{1
################################################################

PeakforestLcmsConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {

	# Check token
	if (is.na(.self$getToken()))
		.self$message(MSG.ERROR, "Peakforest requires a token for this service.")

	if (concatenate)
		url <- paste(.self$getBaseUrl(), 'spectra/lcms/ids/', paste(id, collapse = ','),'?token=', .self$getToken(), sep = '')
	else
		url <- paste(.self$getBaseUrl(), 'spectra/lcms/ids/', id,'?token=', .self$getToken(), sep = '')

	return(url)
})

# Get entry page url {{{1
################################################################

PeakforestLcmsConn$methods( getEntryPageUrl = function(id) {
	return(paste('https://peakforest.org/home?PFs=', id))
})

# Get entry content {{{1
################################################################

PeakforestLcmsConn$methods( getEntryContent = function(id) {
	
	# Initialize contents to return
	content <- rep(NA_character_, length(id))

	# Send request
	jsonstr <- .self$.getUrlScheduler()$getUrl(.self$getEntryContentUrl(id))

	# Parse JSON
	json <- jsonlite::fromJSON(jsonstr, simplifyDataFrame = FALSE)

	if (length(json) > 0) {
		json.ids <- vapply(json, function(x) as.character(x$id), FUN.VALUE = '')
		content[id %in% json.ids] <- vapply(json, function(x) jsonlite::toJSON(x, pretty = TRUE), FUN.VALUE = '')
	}

	return(content)
})

# Create compoud from json {{{1
################################################################

PeakforestLcmsConn$methods( .createPeakforestCompoundFromJSON = function(contents, drop = FALSE) {

	if(is.character(contents))
		contents <- jsonlite::fromJSON(contents, simplifyDataFrame=FALSE)
	
	jsonfields <- list()
	jsonfields[[BIODB.ACCESSION]] <- "id"
	jsonfields[[BIODB.NCBI.PUBCHEM.COMP.ID]] <- "PubChemCID"
	jsonfields[[BIODB.CHEBI.ID]] <- "ChEBI"
	jsonfields[[BIODB.HMDB.METABOLITE.ID]] <- "HMDB"
	jsonfields[[BIODB.KEGG.COMPOUND.ID]] <- "KEGG"
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
		entry <- BiodbEntry$new(.self$getBiodb())
		
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
})

# Create entry {{{1
################################################################

PeakforestLcmsConn$methods( createEntry = function(content, drop = TRUE) {

	# ---------------------------------------------------------
#	entries <- vector(length(content),mode="list")
#	
#	
#	###Checking that it's a list.
#	if(length(content) == 1){
#		if(startsWith(content[[1]], "<html>") ){
#			return(NULL)
#		}else{
#			content <- jsonlite::fromJSON(content[[1]],simplifyDataFrame=FALSE)	
#			
#		}
#	}
#	
#	for (i in seq_along(content)) {
#		
#		single.content <- content[[i]]
#		jsontree <- NULL
#		if(typeof(single.content) == "character"){
#			if(startsWith(single.content, "<html>")|single.content=="null"){
#				entries[[i]] <- NULL
#				next
#			}
#			jsontree <- jsonlite::fromJSON(single.content,simplifyDataFrame=FALSE)
#		}else{
#			jsontree <- content
#		}
#		cnames <- c(BIODB.PEAK.MZ, BIODB.PEAK.RELATIVE.INTENSITY, BIODB.PEAK.FORMULA, BIODB.PEAK.MZTHEO, BIODB.PEAK.ERROR.PPM)
#		
#		entry <- BiodbEntry$new(.self$getBiodb())
#		
#		for(field in names(jsonfields)){
#			
#			tosearch <- jsonfields[[field]]
#			value <- jsontree$tosearch
#			entry$setField(field,value)
#		}
#		
#		######################
#		# TREATING THE PEAKS #
#		######################
#		
#		entry$setField(BIODB.NB.PEAKS,length(jsontree$peaks))
#		peaks <- data.frame( matrix( 0,ncol = length(cnames), nrow = 0))
#		colnames(peaks) <- cnames
#		###Parsing peaks.
#		if(length(jsontree$peaks) != 0){
#			peaks <- sapply(jsontree$peaks,function(x){
#				return(list(as.double(x$mz),
#							as.integer(x$ri),
#							as.character(x$composition),
#							as.double(x$theoricalMass),
#							as.double(x$deltaPPM)
#				))
#			})
#			###Removing all whitespaces from the formule.
#			peaks[3,]<-vapply(peaks[3,],function(x){
#				gsub(" ","",trimws(x))
#			},FUN.VALUE = NA_character_)
#			
#			peaks<-t(peaks)
#			colnames(peaks)<-cnames
#		}
#		
#		entry$setField(BIODB.PEAKS,peaks)
#		
#		##################################
#		# TREATING THE LIST OF COMPOUNDS #
#		##################################
#		
#		entry$setField(BIODB.NB.COMPOUNDS,length(jsontree$listOfCompounds))
#		compounds <- list()
#		
#		###Parsing compounds.
#		if( length( jsontree$listOfCompounds) != 0){
#			compounds <- lapply( jsontree$listOfCompounds, function(x){
#				.self$.createPeakforestCompoundFromJSON(x)
#			})
#		}
#		
#		entry$setField(BIODB.COMPOUNDS, compounds)
#		
#		
#		entries[[i]] <- entry
#	}
#	
#	
#	if (drop && length(content) == 1)
#		entries <- entries[[1]]
})

# Create reduced entry {{{1
################################################################

PeakforestLcmsConn$methods( createReducedEntry = function(content , drop = TRUE){
	entries <- vector(length(content), mode = "list")
	jsonfields <- character()
	
	###Checking that it's a list.
	if (length(content) == 1) {
		if (startsWith(content[[1]], "<html>")) {
			return(NULL)
		} else{
			content <- jsonlite::fromJSON(content[[1]], simplifyDataFrame=FALSE)
			
		}
	}
	if(length(content)==0){
		return(list())
	}
	
	for (i in seq_along(content)) {
		single.content <- content[[i]]
		jsontree <- NULL
		if (typeof(single.content) == "character") {
			if (startsWith(single.content, "<html>") | single.content == "null") {
				entries[[i]] <- NULL
				next
			}
			jsontree <- jsonlite::fromJSON(single.content, simplifyDataFrame=FALSE)
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
		
		entry <- BiodbEntry$new(.self$getBiodb())
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
	
	if (drop && length(content) == 1)
		entries <- entries[[1]]
	
	return(entries)
})

# Search mz range {{{1
################################################################

PeakforestLcmsConn$methods( searchMzRange = function(mzmin, mzmax, rtype = c("object","spec","peak")){
	
	rtype <- match.arg(rtype)
	if(mzmin > mzmax){
		.self$message(MSG.ERROR, "mzmin should be inferior to mzmax.")
	}
	
	url <- paste0("https://rest.peakforest.org/spectra/lcms/peaks/get-range/",mzmin,"/",mzmax)
	
	contents <-  .self$.get.url(url)
	
	jsontree <- fromJSON(contents)
	
	###No match form the output.
	if( length(jsontree)==0 ) return(NULL)
	
	# Getting a list of all the id.
	lid <- sapply(jsontree,function(x){
		x$source$id
	})
	
	# Returning the content for all the spectra
	contents <- .self$getEntryContent(lid)
	
	entries  <- .self$createEntry(contents)
	
	# Checking the return type
	if( rtype=="object" ){
		return( entries )
	}
	
	### XXXX See if we don't want to reduce the output and factorize this shit.
	toreturn <- NULL
	if( rtype=="spec" ){
		toreturn <- sapply(entries,function(x){
			x$getFieldsAsDataFrame()
		})
	}
	if( rtype=="peak" ){
		toreturn <- lapply(entries,function(x){
			temp <- as.data.frame( x$getFieldValue( BIODB.PEAKS ))
			temp$accession = x$getFieldValue( BIODB.ACCESSION) 
			return(temp)
			
		})
	}
	###Trying to convert in data.frame
	if(!is.data.frame(toreturn)){
		temp <- colnames(toreturn[[1]])
		toreturn <- do.call("rbind.fill",toreturn)
		colnames(toreturn) <- temp
	}
	
	return(toreturn)
})

# Search mz tol {{{1
################################################################

PeakforestLcmsConn$methods( searchMzTol = function(mz, tol, tolunit=BIODB.MZTOLUNIT.VALS,
											   rtype = c("object","spec","peak")){
	
	rtype <- match.arg(rtype)
	tolunit <- match.arg(tolunit)
	
	if( tolunit == BIODB.MZTOLUNIT.PPM){
		tol <- tol * mz * 10^-6
	}
	
	mzmin <- mz - tol
	mzmax <- mz + tol
	
	return(.self$searchMzRange(mzmin, mzmax, rtype = rtype))
	
})

# Search for msms spectra precusor around a mass {{{1
################################################################

PeakforestLcmsConn$methods(
	searchSpecPrecTol = function(mz,
								 tol,
								 tolunit = "plain",
								 mode = NULL) {

		largs <- list(token=.self$.token)
		
		if (!is.null(mode)) {
			if (mode %in% c(BIODB.MSMODE.NEG, BIODB.MSMODE.POS)) {
				largs<- c(largs,mode=mode)
			}
		}
		
		if (tolunit == BIODB.MZTOLUNIT.PPM) {
			tol <- tol * mz * 10 ^ -6
		}
		largs<- c(largs,precursorMassDelta=tol)
		
		
		strargs <- apply(rbind(names(largs),as.character(largs)),2,paste,collapse="=")
		strargs <- paste(strargs,collapse = "&")
		##Request which return peak and not spectra.
		url <-
			paste0(.self$geBaseUrl(),
				"/spectra/lcmsms/from-precursor/",
				mz,
				"?",
				strargs
			)
		contents <-  .self$.get.url(url)
		entries  <- .self$createReducedEntry(contents, drop = FALSE)
		return(entries)
	}
)

# Get entry ids {{{1
################################################################

PeakforestLcmsConn$methods( getEntryIds = function(max.results = NA_integer_) {

	# Build URL
	url <- paste(.self$getBaseUrl(), 'spectra/lcms/all/ids?token=', .self$getToken(), sep = '')

	# Send request
	json.str <- .self$.getUrlScheduler()$getUrl(url)

	# Parse JSON
	json <- jsonlite::fromJSON(json.str, simplifyDataFrame = FALSE)

	# Get IDs
	ids <- json

	# Cut
	if ( ! is.na(max.results) && max.results > 1 && max.results < length(ids))
		ids <- ids[1:max.results]

	return(ids)
})

# Get nb entries {{{1
################################################################

PeakforestLcmsConn$methods( getNbEntries = function(count = FALSE) {

	# Build URL
	url <- paste(.self$getBaseUrl(), 'spectra/lcms/all/count?token=', .self$getToken(), sep = '')

	# Send request
	n <- as.integer(.self$.getUrlScheduler()$getUrl(url))

	return(n)
})
