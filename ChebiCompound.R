if ( ! exists('ChebiCompound')) { # Do not load again if already loaded

	source('BiodbEntry.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	ChebiCompound <- setRefClass("ChebiCompound", contains = "BiodbEntry")
	
	###########
	# FACTORY #
	###########
	
	createChebiCompoundFromHtml <- function(contents, drop = TRUE) {

		library(XML)

		compounds <- list()

		# Define xpath expressions
		xpath.expr <- character()
#		xpath.expr[[BIODB.ACCESSION]] <- "//b[starts-with(., 'CHEBI:')]"
		xpath.expr[[BIODB.INCHI]] <- "//td[starts-with(., 'InChI=')]"
		xpath.expr[[BIODB.INCHIKEY]] <- "//td[text()='InChIKey']/../td[2]"

		for (content in contents) {

			# Create instance
			compound <- ChebiCompound$new()

			if ( ! is.null(content) && ! is.na(content)) {
			
				# Parse HTML
				xml <-  htmlTreeParse(content, asText = TRUE, useInternalNodes = TRUE)

				# Test generic xpath expressions
				for (field in names(xpath.expr)) {
					v <- xpathSApply(xml, xpath.expr[[field]], xmlValue)
					if (length(v) > 0)
						compound$setField(field, v)
				}
			
				# Get accession
				accession <- xpathSApply(xml, "//b[starts-with(., 'CHEBI:')]", xmlValue)
				if (length(accession) > 0) {
					accession <- sub('^CHEBI:([0-9]+)$', '\\1', accession, perl = TRUE)
					compound$setField(BIODB.ACCESSION, accession)
				}
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
