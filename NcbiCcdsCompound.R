if ( ! exists('NcbiccdsCompound')) { # Do not load again if already loaded

	source('BiodbEntry.R')

	#####################
	# CLASS DECLARATION #
	#####################

	NcbiccdsCompound <- setRefClass("NcbiccdsCompound", contains = "BiodbEntry")

	###########
	# FACTORY #
	###########

	createNcbiccdsCompoundFromHtml <- function(contents, drop = TRUE) {

		library(XML)

		compounds <- list()

		for (html in contents) {

			# Create instance
			compound <- NcbiccdsCompound$new()
		
			# Parse HTML
			xml <-  htmlTreeParse(html, asText = TRUE, useInternalNodes = TRUE)

			if (length(getNodeSet(xml, "//*[starts-with(.,'No results found for CCDS ID ')]")) == 0) {
				compound$setField(BIODB.ACCESSION, xpathSApply(xml, "//input[@id='DATA']", xmlGetAttr, "value"))
				compound$setField(BIODB.SEQUENCE, xpathSApply(xml, "//b[starts-with(.,'Nucleotide Sequence')]/../tt", xmlValue))
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
