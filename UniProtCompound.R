if ( ! exists('UniprotCompound')) { # Do not load again if already loaded

	source('BiodbEntry.R')

	#####################
	# CLASS DECLARATION #
	#####################

	UniprotCompound <- setRefClass("UniprotCompound", contains = "BiodbEntry")

	###########
	# FACTORY #
	###########

	createUniprotCompoundFromXml <- function(contents, drop = FALSE) {

		library(XML)

		# Set XML namespace
		ns <- c(uniprot = "http://uniprot.org/uniprot")

		compounds <- list()

		# Define xpath expressions
		xpath.values <- character()
		xpath.values[[BIODB.NAME]] <- "/uniprot:uniprot/uniprot:compound/uniprot:name"
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
			compound <- HmdbCompound$new()

			# If the entity doesn't exist (i.e.: no <id>.xml page), then it returns an HTML page
			if ( ! grepl("^<!DOCTYPE html ", content, perl = TRUE)) {

				# Parse XML
				xml <-  xmlInternalTreeParse(content, asText = TRUE)

				# Test value xpath
				for (field in names(xpath.values)) {
					v <- xpathSApply(xml, xpath.values[[field]], xmlValue, namespaces = ns)
					if (length(v) > 0)
						compound$setField(field, v)
				}

				# Test attribute  xpath
				for (field in names(xpath.attr)) {
					v <- xpathSApply(xml, xpath.attr[[field]]$path, xmlGetAttr, xpath.attr[[field]]$attr, namespaces = ns)
					if (length(v) > 0)
						compound$setField(field, v)
				}

				# Remove new lines from sequence string
				seq <- compound$getField(BIODB.SEQUENCE)
				if ( ! is.na(seq))
					compound$setField(BIODB.SEQUENCE, gsub("\\n", "", seq))
			}

			compounds <- c(compounds, compound)
		}

		# Replace elements with no accession id by NULL
		compounds <- lapply(compounds, function(x) if (is.na(x$getField(BIODB.ACCESSION))) NULL else x)

		# If the input was a single element, then output a single object
		if (drop && length(contents) == 1)
			compounds <- compounds[[1]]
	
		return(compounds)
	}
}
