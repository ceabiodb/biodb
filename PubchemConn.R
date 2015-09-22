if ( ! exists('get.pubchem.entry.url')) { # Do not load again if already loaded

	#########################
	# GET PUBCHEM ENTRY URL #
	#########################
	
	get.pubchem.entry.url <- function(id) {
	
		id <- gsub(' ', '', id, perl = TRUE)
		id <- gsub('^CID', '', id, perl = TRUE)
		url <- paste0('http://pubchem.ncbi.nlm.nih.gov/compound/', id)
	
		return(url)
	}
	
} # end of load safe guard
