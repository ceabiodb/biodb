if ( ! exists('MirbaseCompound')) { # Do not load again if already loaded

	source('BiodbEntry.R')

	#####################
	# CLASS DECLARATION #
	#####################

	MirbaseCompound <- setRefClass("MirbaseCompound", contains = "BiodbEntry")

	###########
	# FACTORY #
	###########

	createMirbaseCompoundFromHtml <- function(contents, drop = TRUE) {

		library(XML)

		compounds <- list()
	
		# Define fields regex
		xpath.expr <- character()
		xpath.expr[[BIODB.ACCESSION]]  <- "//td[text()='Accession number']/../td[2]"
		xpath.expr[[BIODB.NAME]]       <- "//td[text()='ID']/../td[2]"
		xpath.expr[[BIODB.SEQUENCE]]   <- "//td[text()='Sequence']/..//pre"

		for (html in contents) {

			# Create instance
			compound <- ChebiCompound$new()
		
			# Parse HTML
			xml <-  htmlTreeParse(html, asText = TRUE, useInternalNodes = TRUE)

			# Test generic xpath expressions
			for (field in names(xpath.expr)) {
				v <- xpathSApply(xml, xpath.expr[[field]], xmlValue)
				if (length(v) > 0)
					compound$setField(field, v)
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
