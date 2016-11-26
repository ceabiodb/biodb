if ( ! exists('NcbiccdsEntry')) { # Do not load again if already loaded

	#####################
	# CLASS DECLARATION #
	#####################

	NcbiccdsEntry <- setRefClass("NcbiccdsEntry", contains = "BiodbEntry")

	###########
	# FACTORY #
	###########

	createNcbiccdsEntryFromHtml <- function(contents, drop = TRUE) {

		library(XML)

		entries <- list()

		for (html in contents) {

			# Create instance
			entry <- NcbiccdsEntry$new()
		
			# Parse HTML
			xml <-  htmlTreeParse(html, asText = TRUE, useInternalNodes = TRUE)

			if (length(getNodeSet(xml, "//*[starts-with(.,'No results found for CCDS ID ')]")) == 0) {
				entry$setField(BIODB.ACCESSION, xpathSApply(xml, "//input[@id='DATA']", xmlGetAttr, "value"))
				entry$setField(BIODB.SEQUENCE, xpathSApply(xml, "//b[starts-with(.,'Nucleotide Sequence')]/../tt", xmlValue))
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
}
