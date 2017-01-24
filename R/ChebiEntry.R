#####################
# CLASS DECLARATION #
#####################

ChebiEntry <- methods::setRefClass("ChebiEntry", contains = "BiodbEntry")

###########
# FACTORY #
###########

createChebiEntryFromHtml <- function(biodb, contents, drop = TRUE) {

	entries <- list()

	# Define xpath expressions
	xpath.expr <- character()
#		xpath.expr[[BIODB.ACCESSION]] <- "//b[starts-with(., 'CHEBI:')]"
	xpath.expr[[BIODB.INCHI]] <- "//td[starts-with(., 'InChI=')]"
	xpath.expr[[BIODB.INCHIKEY]] <- "//td[text()='InChIKey']/../td[2]"

	for (content in contents) {

		# Create instance
		entry <- ChebiEntry$new(biodb)

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
			accession <- XML::xpathSApply(xml, "//b[starts-with(., 'CHEBI:')]", XML::xmlValue)
			if (length(accession) > 0) {
				accession <- sub('^CHEBI:([0-9]+)$', '\\1', accession, perl = TRUE)
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
