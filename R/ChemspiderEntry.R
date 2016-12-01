library(XML)
library(methods)

#####################
# CLASS DECLARATION #
#####################

ChemspiderEntry <- methods::setRefClass("ChemspiderEntry", contains = "BiodbEntry")

############################
# CREATE COMPOUND FROM XML #
############################

createChemspiderEntryFromXml <- function(contents, drop = TRUE) {

	entries <- list()

	# Define xpath expressions
	xpath.expr <- character()
	xpath.expr[[BIODB.ACCESSION]]    	<- "//CSID"
	xpath.expr[[BIODB.FORMULA]]      	<- "//MF"
	xpath.expr[[BIODB.NAME]]         	<- "//CommonName"
	xpath.expr[[BIODB.AVERAGE.MASS]] 	<- "//AverageMass"
	xpath.expr[[BIODB.INCHI]]           <- "//InChI"
	xpath.expr[[BIODB.INCHIKEY]]       	<- "//InChIKey"
	xpath.expr[[BIODB.SMILES]]          <- "//SMILES"

	for (content in contents) {

		# Create instance
		entry <- ChemspiderEntry$new()

		if ( ! is.null(content) && ! is.na(content) && content != 'NA') {
		
			# Parse XML
			xml <-  XML::xmlInternalTreeParse(content, asText = TRUE)

			# Test generic xpath expressions
			for (field in names(xpath.expr)) {
				v <- XML::xpathSApply(xml, xpath.expr[[field]], XML::xmlValue)
				if (length(v) > 0)
					entry$setField(field, v)
			}
		}

		entries <- c(entries, entry)
	}

	# Replace elements with no accession id by NULL
	entries <- lapply(entries, function(x) if (is.na(x$getField(BIODB.ACCESSION))) NULL else x)

	# If the input was a single element, then output a single object
	if (drop && length(contents) == 1)
		entries <- entries[[1]]

	return(entries)
}

#############################
# CREATE COMPOUND FROM HTML #
#############################

createChemspiderEntryFromHtml <- function(contents, drop = TRUE) {

	entries <- list()

	# Define xpath expressions
	xpath.expr <- character()

	for (content in contents) {

		# Create instance
		entry <- ChemspiderEntry$new()

		if ( ! is.null(content) && ! is.na(content)) {
		
			# Parse HTML
			xml <-  XML::htmlTreeParse(content, asText = TRUE, useInternalNodes = TRUE)

			# Test generic xpath expressions
			for (field in names(xpath.expr)) {
				v <- XML::xpathSApply(xml, xpath.expr[[field]], XML::xmlValue)
				if (length(v) > 0)
					entry$setField(field, v)
			}
		
			# Get accession
			accession <- XML::xpathSApply(xml, "//li[starts-with(., 'ChemSpider ID')]", XML::xmlValue)
			if (length(accession) > 0) {
				accession <- sub('^ChemSpider ID([0-9]+)$', '\\1', accession, perl = TRUE)
				entry$setField(BIODB.ACCESSION, accession)
			}
		}

		entries <- c(entries, entry)
	}

	# Replace elements with no accession id by NULL
	entries <- lapply(entries, function(x) if (is.na(x$getField(BIODB.ACCESSION))) NULL else x)

	# If the input was a single element, then output a single object
	if (drop && length(contents) == 1)
		entries <- entries[[1]]

	return(entries)
}
