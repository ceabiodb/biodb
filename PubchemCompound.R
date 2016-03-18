if ( ! exists('PubchemCompound')) { # Do not load again if already loaded

	library(XML)
	source('BiodbEntry.R')

	#####################
	# CLASS DECLARATION #
	#####################

	PubchemCompound <- setRefClass("PubchemCompound", contains = "BiodbEntry")

	###########
	# FACTORY #
	###########

	createPubchemCompoundFromXml <- function(contents, drop = TRUE) {

		compounds <- list()

		# Set XML namespace
		ns <- c(pubchem = "http://pubchem.ncbi.nlm.nih.gov/pug_view")

		# Define xpath expressions
		xpath.expr <- character()
		xpath.expr[[RBIODB.ACCESSION]] <- "//pubchem:RecordType[text()='CID']/../pubchem:RecordNumber"
		xpath.expr[[RBIODB.INCHI]] <- "//pubchem:Name[text()='InChI']/../pubchem:StringValue"
		xpath.expr[[RBIODB.INCHIKEY]] <- "//pubchem:Name[text()='InChI Key']/../pubchem:StringValue"

		for (content in contents) {

			# Create instance
			compound <- PubchemCompound$new()

			# Parse XML
			xml <-  xmlInternalTreeParse(content, asText = TRUE)

			# Unknown compound
			fault <- xpathSApply(xml, "/pubchem:Fault", xmlValue, namespaces = ns)
			if (length(fault) == 0) {

				# Test generic xpath expressions
				for (field in names(xpath.expr)) {
					v <- xpathSApply(xml, xpath.expr[[field]], xmlValue, namespaces = ns)
					if (length(v) > 0)
						compound$setField(field, v)
				}

				# Get name
				name <- NA_character_
				tryCatch( { name <- xpathSApply(xml, "//pubchem:Name[text()='IUPAC Name']/../pubchem:StringValue", xmlValue, namespaces = ns) }, warning = function(w) {})
				if (is.na(name))
					tryCatch( { name <- xpathSApply(xml, "//pubchem:Name[text()='Record Title']/../pubchem:StringValue", xmlValue, namespaces = ns) }, warning = function(w) {})
				if ( ! is.na(name))
					compound$setField(RBIODB.NAME, name)

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
