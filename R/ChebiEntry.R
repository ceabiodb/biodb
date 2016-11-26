if ( ! exists('ChebiEntry')) { # Do not load again if already loaded

	#####################
	# CLASS DECLARATION #
	#####################
	
	ChebiEntry <- setRefClass("ChebiEntry", contains = "BiodbEntry")
	
	###########
	# FACTORY #
	###########
	
	createChebiEntryFromHtml <- function(contents, drop = TRUE) {

		library(XML)

		entries <- list()

		# Define xpath expressions
		xpath.expr <- character()
#		xpath.expr[[BIODB.ACCESSION]] <- "//b[starts-with(., 'CHEBI:')]"
		xpath.expr[[BIODB.INCHI]] <- "//td[starts-with(., 'InChI=')]"
		xpath.expr[[BIODB.INCHIKEY]] <- "//td[text()='InChIKey']/../td[2]"

		for (content in contents) {

			# Create instance
			entry <- ChebiEntry$new()

			if ( ! is.null(content) && ! is.na(content)) {
			
				# Parse HTML
				xml <-  htmlTreeParse(content, asText = TRUE, useInternalNodes = TRUE)

				# Test generic xpath expressions
				for (field in names(xpath.expr)) {
					v <- xpathSApply(xml, xpath.expr[[field]], xmlValue)
					if (length(v) > 0)
						entry$setField(field, v)
				}
			
				# Get accession
				accession <- xpathSApply(xml, "//b[starts-with(., 'CHEBI:')]", xmlValue)
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
}
