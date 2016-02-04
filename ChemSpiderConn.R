if ( ! exists('get.chemspider.image.url')) { # Do not load again if already loaded

	############################
	# GET CHEMSPIDER COMPOUND URL #
	############################
	
	get.chemspider.compound.url <- function(id, type = RBIODB.HTML) {
	
		url <- paste0('http://www.chemspider.com/Chemical-Structure.', id, '.html')

		return(url)
	}

	############################
	# GET CHEMSPIDER IMAGE URL #
	############################
	
	get.chemspider.image.url <- function(id) {
	
		url <- paste0('http://www.chemspider.com/ImagesHandler.ashx?w=300&h=300&id=', id)

		return(url)
	}
	
} # end of load safe guard

