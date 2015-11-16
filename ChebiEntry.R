library(XML)
source('BioDbEntry.R')

#####################
# CLASS DECLARATION #
#####################

ChebiEntry <- setRefClass("ChebiEntry", contains = "BioDbEntry", fields = list(.id = "character"))

###############
# CONSTRUCTOR #
###############

ChebiEntry$methods( initialize = function(id = NA_character_, ...) {

		.id <<- id

		callSuper(...)
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
	id <- sub('^CHEBI:([0-9]+)$', '\\1', id, perl = TRUE)

	# Create entry
	entry <- if ( ! is.na(id)) ChebiEntry$new(id = id)

	return(entry)
}
