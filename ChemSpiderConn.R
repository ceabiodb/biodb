if ( ! exists('ChemspiderConn')) { # Do not load again if already loaded

	source('BiodbConn.R')
	source('ChemspiderCompound.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	ChemspiderConn <- setRefClass("ChemspiderConn", contains = "BiodbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	ChemspiderConn$methods( getEntryContentType = function(type) {
		return(RBIODB.HTML)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	ChemspiderConn$methods( getEntryContent = function(type, id) {

		if (type == RBIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(id))

			# Request
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.chemspider.compound.url(x)), FUN.VALUE = '')

			return(content)
		}

		return(NULL)
	})
	
	################
	# CREATE ENTRY #
	################
	
	ChemspiderConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == RBIODB.COMPOUND) createChemspiderCompoundFromHtml(content, drop = drop) else NULL)
	})

	###############################
	# GET CHEMSPIDER COMPOUND URL #
	###############################
	
	get.chemspider.compound.url <- function(id) {
	
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

