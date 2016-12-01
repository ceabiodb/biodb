#####################
# CLASS DECLARATION #
#####################

NcbiccdsEntry <- methods::setRefClass("NcbiccdsEntry", contains = "BiodbEntry")

###########
# FACTORY #
###########

createNcbiccdsEntryFromHtml <- function(contents, drop = TRUE) {

	entries <- list()

	for (html in contents) {

		# Create instance
		entry <- NcbiccdsEntry$new()
	
		# Parse HTML
		xml <-  XML::htmlTreeParse(html, asText = TRUE, useInternalNodes = TRUE)

		if (length(XML::getNodeSet(xml, "//*[starts-with(.,'No results found for CCDS ID ')]")) == 0) {
			entry$setField(BIODB.ACCESSION, XML::xpathSApply(xml, "//input[@id='DATA']", XML::xmlGetAttr, "value"))
			entry$setField(BIODB.SEQUENCE, XML::xpathSApply(xml, "//b[starts-with(.,'Nucleotide Sequence')]/../tt", XML::xmlValue))
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
