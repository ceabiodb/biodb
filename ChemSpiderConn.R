if ( ! exists('ChemspiderConn')) { # Do not load again if already loaded

	source('RemotedbConn.R')
	source('ChemspiderCompound.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	ChemspiderConn <- setRefClass("ChemspiderConn", contains = "RemotedbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	ChemspiderConn$methods( getEntryContentType = function(type) {
		return(BIODB.XML)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	ChemspiderConn$methods( getEntryContent = function(type, id) {

		if (type == BIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(id))

			# Request
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.entry.url(BIODB.CHEMSPIDER, x)), FUN.VALUE = '')

			# Split content

			return(content)
		}

		return(NULL)
	})

	################
	# CREATE ENTRY #
	################
	
	ChemspiderConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == BIODB.COMPOUND) createChemspiderCompoundFromXml(content, drop = drop) else NULL)
	})

	############################
	# GET CHEMSPIDER IMAGE URL #
	############################
	
	get.chemspider.image.url <- function(id) {
	
		url <- paste0('http://www.chemspider.com/ImagesHandler.ashx?w=300&h=300&id=', id)

		return(url)
	}
	
} # end of load safe guard

