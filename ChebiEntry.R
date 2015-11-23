library(XML)
source('BioDbEntry.R')

#####################
# CLASS DECLARATION #
#####################

ChebiEntry <- setRefClass("ChebiEntry", contains = "BioDbEntry", fields = list(.inchi = "character", .inchikey = "character"))

###############
# CONSTRUCTOR #
###############

ChebiEntry$methods( initialize = function(id = NA_character_, inchi = NA_character_, inchikey = NA_character_, ...) {

	.inchi <<- if ( ! is.null(inchi)) inchi else NA_character_
	.inchikey <<- if ( ! is.null(inchikey)) inchikey else NA_character_

	callSuper(id = id, ...)
})

#########
# INCHI #
#########

ChebiEntry$methods(	getInchi = function() {
	return(.self$.inchi)
})

#############
# INCHI KEY #
#############

ChebiEntry$methods(	getInchiKey = function() {
	return(.self$.inchikey)
})

###########
# FACTORY #
###########

createChebiEntryFromHtml <- function(htmlstr) {

	entry <- NULL

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

		# Create entry
		entry <- ChebiEntry$new(id = id, inchi = inchi, inchikey = inchikey)
	}

	return(entry)
}
