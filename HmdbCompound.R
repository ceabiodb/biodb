if ( ! exists('HmdbCompound')) { # Do not load again if already loaded

	source('BiodbEntry.R')

	#####################
	# CLASS DECLARATION #
	#####################

	HmdbCompound <- setRefClass("HmdbCompound", contains = "BiodbEntry")

	###########
	# FACTORY #
	###########

	createHmdbCompoundFromXml <- function(contents, drop = FALSE) {

		library(XML)

		compounds <- list()

		# Define xpath expressions
		xpath.expr <- character()
		xpath.expr[[BIODB.ACCESSION]]          <- "/metabolite/accession"
		xpath.expr[[BIODB.KEGG.ID]]            <- "//kegg_id"
		xpath.expr[[BIODB.NAME]]               <- "/metabolite/name"
		xpath.expr[[BIODB.FORMULA]]            <- "/metabolite/chemical_formula"
		xpath.expr[[BIODB.SUPER.CLASS]]        <- "//super_class"
		xpath.expr[[BIODB.AVERAGE.MASS]]       <- "//average_molecular_weight"
		xpath.expr[[BIODB.MONOISOTOPIC.MASS]]  <- "//monisotopic_moleculate_weight"

		for (content in contents) {

			# Create instance
			compound <- HmdbCompound$new()

			# Parse XML
			xml <-  xmlInternalTreeParse(content, asText = TRUE)

			# An error occured
			if (length(getNodeSet(xml, "//error")) == 0) {

				# Test generic xpath expressions
				for (field in names(xpath.expr)) {
					v <- xpathSApply(xml, xpath.expr[[field]], xmlValue)
					if (length(v) > 0)
						compound$setField(field, v)
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
