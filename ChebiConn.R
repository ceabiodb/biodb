if ( ! exists('get.chebi.entry.url')) { # Do not load again if already loaded

	#######################
	# GET CHEBI ENTRY URL #
	#######################
	
	get.chebi.entry.url <- function(id) {
	
		url <- paste0('https://www.ebi.ac.uk/chebi/searchId.do?chebiId=', id)
	
		return(url)
	}

} # end of load safe guard
