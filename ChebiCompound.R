if ( ! exists('ChebiCompound')) { # Do not load again if already loaded

	library(XML)
	source('BiodbCompound.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	ChebiCompound <- setRefClass("ChebiCompound", contains = "BiodbCompound", fields = list(.inchi = "character", .inchikey = "character"))
	
	###############
	# CONSTRUCTOR #
	###############
	
	ChebiCompound$methods( initialize = function(id = NA_character_, inchi = NA_character_, inchikey = NA_character_, ...) {
	
		.inchi <<- if ( ! is.null(inchi)) inchi else NA_character_
		.inchikey <<- if ( ! is.null(inchikey)) inchikey else NA_character_
	
		callSuper(id = id, ...)
	})
	
	#########
	# INCHI #
	#########
	
	ChebiCompound$methods(	getInchi = function() {
		return(.self$.inchi)
	})
	
	#############
	# INCHI KEY #
	#############
	
	ChebiCompound$methods(	getInchiKey = function() {
		return(.self$.inchikey)
	})
	
	###########
	# FACTORY #
	###########
	
	createChebiCompoundFromHtml <- function(htmlstr) {
	
		compound <- NULL
	
		# Parse HTML
		xml <-  htmlTreeParse(htmlstr, asText = TRUE, useInternalNodes = TRUE)
	
		# Get ID
		id <- xpathSApply(xml, "//b[starts-with(., 'CHEBI:')]", xmlValue)
		if (length(id) > 0)
			id <- sub('^CHEBI:([0-9]+)$', '\\1', id, perl = TRUE)
		else
			id <- NA_character_
	
		if ( ! is.na(id)) {	
			# Get InChI
			inchi <- xpathSApply(xml, "//td[starts-with(., 'InChI=')]", xmlValue)
	
			# Get InChI KEY
			inchikey <- xpathSApply(xml, "//td[text()='InChIKey']/../td[2]", xmlValue)
	
			# Create compound
			compound <- ChebiCompound$new(id = id, inchi = inchi, inchikey = inchikey)
		}
	
		return(compound)
	}
}
