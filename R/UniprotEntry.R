#####################
# CLASS DECLARATION #
#####################

UniprotEntry <- methods::setRefClass("UniprotEntry", contains = "BiodbEntry")

###########
# FACTORY #
###########

createUniprotEntryFromXml <- function(contents, drop = FALSE) {

	# Set XML namespace
	ns <- c(uniprot = "http://uniprot.org/uniprot")

	entries <- list()

	# Define xpath expressions
	xpath.values <- character()
	xpath.values[[BIODB.NAME]] <- "/uniprot:uniprot/uniprot:entry/uniprot:name"
	xpath.values[[BIODB.GENE.SYMBOLS]] <- "//uniprot:gene/uniprot:name"
	xpath.values[[BIODB.FULLNAMES]] <- "//uniprot:protein//uniprot:fullName"
	xpath.values[[BIODB.SEQUENCE]] <- "//uniprot:entry/uniprot:sequence"
	xpath.values[[BIODB.ACCESSION]] <- "//uniprot:accession[1]"
	xpath.attr <- list()
	xpath.attr[[BIODB.KEGG.ID]] <- list(path = "//uniprot:dbReference[@type='KEGG']", attr = 'id')
	xpath.attr[[BIODB.NCBI.GENE.ID]] <- list(path = "//uniprot:dbReference[@type='GeneID']", attr = 'id')
	xpath.attr[[BIODB.ENZYME.ID]] <- list(path = "//uniprot:dbReference[@type='EC']", attr = 'id')
	xpath.attr[[BIODB.MASS]] <- list(path = "//uniprot:entry/uniprot:sequence", attr = 'mass')
	xpath.attr[[BIODB.LENGTH]] <- list(path = "//uniprot:entry/uniprot:sequence", attr = 'length')

	for (content in contents) {

		# Create instance
		entry <- UniprotEntry$new()

		# If the entity doesn't exist (i.e.: no <id>.xml page), then it returns an HTML page
		if ( ! grepl("^<!DOCTYPE html ", content, perl = TRUE)) {

			# Parse XML
			xml <-  XML::xmlInternalTreeParse(content, asText = TRUE)

			# Test value xpath
			for (field in names(xpath.values)) {
				v <- XML::xpathSApply(xml, xpath.values[[field]], XML::xmlValue, namespaces = ns)
				if (length(v) > 0)
					entry$setField(field, v)
			}

			# Test attribute  xpath
			for (field in names(xpath.attr)) {
				v <- XML::xpathSApply(xml, xpath.attr[[field]]$path, XML::xmlGetAttr, xpath.attr[[field]]$attr, namespaces = ns)
				if (length(v) > 0)
					entry$setField(field, v)
			}

			# Remove new lines from sequence string
			seq <- entry$getField(BIODB.SEQUENCE)
			if ( ! is.na(seq))
				entry$setField(BIODB.SEQUENCE, gsub("\\n", "", seq))
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
