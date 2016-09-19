if ( ! exists('MirbaseEntry')) {

	source('BiodbEntry.R')

	#####################
	# CLASS DECLARATION #
	#####################

	MirbaseEntry <- setRefClass("MirbaseEntry", contains = "BiodbEntry")

	###########
	# FACTORY #
	###########

	createMirbaseEntryFromHtml <- function(contents, drop = TRUE) {

		library(XML)

		entries <- list()
	
		# Define fields regex
		xpath.expr <- character()
		xpath.expr[[BIODB.ACCESSION]]  <- "//td[text()='Accession number']/../td[2]"
		xpath.expr[[BIODB.NAME]]       <- "//td[text()='ID']/../td[2]"
		xpath.expr[[BIODB.SEQUENCE]]   <- "//td[text()='Sequence']/..//pre"

		for (html in contents) {

			# Create instance
			entry <- ChebiCompound$new()
		
			# Parse HTML
			xml <-  htmlTreeParse(html, asText = TRUE, useInternalNodes = TRUE)

			# Test generic xpath expressions
			for (field in names(xpath.expr)) {
				v <- xpathSApply(xml, xpath.expr[[field]], xmlValue)
				if (length(v) > 0)
					entry$setField(field, v)
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
