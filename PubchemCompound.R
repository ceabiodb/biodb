library(XML)
source('BiodbCompound.R')

#####################
# CLASS DECLARATION #
#####################

PubchemCompound <- setRefClass("PubchemCompound", contains = "BiodbCompound", fields = list(.inchi = "character", .inchikey = "character", .name = "character"))

###############
# CONSTRUCTOR #
###############

PubchemCompound$methods( initialize = function(id = NA_character_, inchi = NA_character_, inchikey = NA_character_, name = NA_character_, ...) {

	.inchi <<- if ( ! is.null(inchi)) inchi else NA_character_
	.inchikey <<- if ( ! is.null(inchikey)) inchikey else NA_character_
	.name <<- if ( ! is.null(name)) name else NA_character_

	callSuper(id = id, ...)
})

########
# NAME #
########

PubchemCompound$methods( getName = function() {
	return(.self$.name)
})

#########
# INCHI #
#########

PubchemCompound$methods(	getInchi = function() {
	return(.self$.inchi)
})

#############
# INCHI KEY #
#############

PubchemCompound$methods(	getInchiKey = function() {
	return(.self$.inchikey)
})

###########
# FACTORY #
###########

createPubchemCompoundFromXml <- function(xmlstr) {

	compound <- NULL

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

			# Get name
			name <- NA_character_
			tryCatch( { name <- xpathSApply(xml, "//pubchem:Name[text()='IUPAC Name']/../pubchem:StringValue", xmlValue, namespaces = ns) }, warning = function(w) {})
			if (is.na(name))
				tryCatch( { name <- xpathSApply(xml, "//pubchem:Name[text()='Record Title']/../pubchem:StringValue", xmlValue, namespaces = ns) }, warning = function(w) {})

			# Get InChI
			inchi <- xpathSApply(xml, "//pubchem:Name[text()='InChI']/../pubchem:StringValue", xmlValue, namespaces = ns)

			# Get InChI KEY
			inchikey <- xpathSApply(xml, "//pubchem:Name[text()='InChI Key']/../pubchem:StringValue", xmlValue, namespaces = ns)

			# Create compound
			compound <- PubchemCompound$new(id = id, inchi = inchi, inchikey = inchikey, name = name)
		}
	}

	return(compound)
}
