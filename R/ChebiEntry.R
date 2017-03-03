#####################
# CLASS DECLARATION #
#####################

ChebiEntry <- methods::setRefClass("ChebiEntry", contains = "BiodbEntry")

###########
# FACTORY #
###########

createChebiEntryFromXml <- function(biodb, content, drop = TRUE) {

	entries <- list()

	# Define xpath expressions
	xpath.expr <- character()
	xpath.expr[[BIODB.SMILES]] <- "//chebi:return/chebi:smiles"
	xpath.expr[[BIODB.INCHI]] <- "//chebi:return/chebi:inchi"
	xpath.expr[[BIODB.INCHIKEY]] <- "//chebi:return/chebi:inchiKey"
	xpath.expr[[BIODB.KEGG.COMPOUND.ID]] <- "//chebi:DatabaseLinks/chebi:type[text()='KEGG COMPOUND accession']/../chebi:data"
	xpath.expr[[BIODB.MASS]] <- "//chebi:mass"
	xpath.expr[[BIODB.MONOISOTOPIC.MASS]] <- "//chebi:monoisotopicMass"
	xpath.expr[[BIODB.CHARGE]] <- "//chebi:charge"

	ns <- c(chebi = "http://www.ebi.ac.uk/webservices/chebi")

	for (single.content in content) {

		# Create instance
		entry <- ChebiEntry$new(biodb)

		if ( ! is.null(single.content) && ! is.na(single.content)) {
		
			# Parse XML
			xml <-  XML::xmlInternalTreeParse(single.content, asText = TRUE)

			# Test generic xpath expressions
			for (field in names(xpath.expr)) {
				v <- XML::xpathSApply(xml, xpath.expr[[field]], XML::xmlValue, namespaces = ns)
				if (length(v) > 0)
					entry$setField(field, v)
			}
		
			# Get accession
			accession <- XML::xpathSApply(xml, "//chebi:return/chebi:chebiId", XML::xmlValue, namespaces = ns)
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
	if (drop && length(content) == 1)
		entries <- entries[[1]]

	return(entries)
}
