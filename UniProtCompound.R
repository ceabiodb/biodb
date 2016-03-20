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
		xpath.values[[RBIODB.NAME]] <- "/uniprot:uniprot/uniprot:compound/uniprot:name"
		xpath.values[[RBIODB.GENE.SYMBOLS]] <- "//uniprot:gene/uniprot:name"
		xpath.values[[RBIODB.FULLNAMES]] <- "//uniprot:protein//uniprot:fullName"
		xpath.values[[RBIODB.SEQUENCE]] <- "//uniprot:entry/uniprot:sequence"
		xpath.values[[RBIODB.ACCESSION]] <- "//uniprot:accession[1]"
		xpath.attr <- list()
		xpath.attr[[RBIODB.KEGG.ID]] <- list(path = "//uniprot:dbReference[@type='KEGG']", attr = 'id')
		xpath.attr[[RBIODB.NCBI.GENE.ID]] <- list(path = "//uniprot:dbReference[@type='GeneID']", attr = 'id')
		xpath.attr[[RBIODB.ENZYME.ID]] <- list(path = "//uniprot:dbReference[@type='EC']", attr = 'id')
		xpath.attr[[RBIODB.MASS]] <- list(path = "//uniprot:entry/uniprot:sequence", attr = 'mass')
		xpath.attr[[RBIODB.LENGTH]] <- list(path = "//uniprot:entry/uniprot:sequence", attr = 'length')

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
					print(v)
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
				seq <- compound$getField(RBIODB.SEQUENCE)
				if ( ! is.na(seq))
					compound$setField(RBIODB.SEQUENCE, gsub("\\n", "", seq))
			}

			compounds <- c(compounds, compound)
		}

		# Replace elements with no accession id by NULL
		compounds <- lapply(compounds, function(x) if (is.na(x$getField(RBIODB.ACCESSION))) NULL else x)

		# If the input was a single element, then output a single object
		if (drop && length(contents) == 1)
			compounds <- compounds[[1]]
	
		return(compounds)
	}
}
