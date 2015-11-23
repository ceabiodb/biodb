library(XML)
source('BiodbEntry.R')

#####################
# CLASS DECLARATION #
#####################

PubchemEntry <- setRefClass("PubchemEntry", contains = "BiodbEntry", fields = list(.inchi = "character", .inchikey = "character"))

###############
# CONSTRUCTOR #
###############

PubchemEntry$methods( initialize = function(id = NA_character_, inchi = NA_character_, inchikey = NA_character_, ...) {

	.inchi <<- if ( ! is.null(inchi)) inchi else NA_character_
	.inchikey <<- if ( ! is.null(inchikey)) inchikey else NA_character_

	callSuper(id = id, ...)
})

#########
# INCHI #
#########

PubchemEntry$methods(	getInchi = function() {
	return(.self$.inchi)
})

#############
# INCHI KEY #
#############

PubchemEntry$methods(	getInchiKey = function() {
	return(.self$.inchikey)
})

###########
# FACTORY #
###########

createPubchemEntryFromXml <- function(xmlstr) {

	entry <- NULL

	# Set XML namespace
	ns <- c(pubchem = "http://pubchem.ncbi.nlm.nih.gov/pug_view")

	# Parse XML
	xml <-  xmlInternalTreeParse(xmlstr, asText = TRUE)

	# Unknown compound
	fault <- xpathSApply(xml, "/pubchem:Fault", xmlValue, namespaces = ns)
	if (length(fault) == 0) {

		# Get ID
		id <- xpathSApply(xml, "//pubchem:RecordType[text()='CID']/../pubchem:RecordNumber", xmlValue, namespaces = ns)

		if ( ! is.na(id)) {	
			# Get InChI
			inchi <- xpathSApply(xml, "//pubchem:Name[text()='InChI']/../pubchem:StringValue", xmlValue, namespaces = ns)

			# Get InChI KEY
			inchikey <- xpathSApply(xml, "//pubchem:Name[text()='InChI Key']/../pubchem:StringValue", xmlValue, namespaces = ns)

			# Create entry
			entry <- PubchemEntry$new(id = id, inchi = inchi, inchikey = inchikey)
		}
	}

	return(entry)
}
