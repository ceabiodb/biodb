#####################
# CLASS DECLARATION #
#####################

HmdbEntry <- methods::setRefClass("HmdbEntry", contains = "BiodbEntry")

###########
# FACTORY #
###########

createHmdbEntryFromXml <- function(biodb, contents, drop = FALSE) {

	entries <- list()

	# Define xpath expressions
	xpath.expr <- character()
	xpath.expr[[BIODB.ACCESSION]]          <- "/metabolite/accession"
	xpath.expr[[BIODB.KEGGCOMPOUND.ID]]    <- "//kegg_id"
	xpath.expr[[BIODB.NAME]]               <- "/metabolite/name"
	xpath.expr[[BIODB.FORMULA]]            <- "/metabolite/chemical_formula"
	xpath.expr[[BIODB.SUPER.CLASS]]        <- "//super_class"
	xpath.expr[[BIODB.AVERAGE.MASS]]       <- "//average_molecular_weight"
	xpath.expr[[BIODB.MONOISOTOPIC.MASS]]  <- "//monisotopic_moleculate_weight"

	for (content in contents) {

		# Create instance
		entry <- HmdbEntry$new(biodb)

		if ( ! is.null(content) && ! is.na(content)) {

			# Parse XML
			xml <-  XML::xmlInternalTreeParse(content, asText = TRUE)

			# An error occured
			if (length(XML::getNodeSet(xml, "//error")) == 0) {

				# Test generic xpath expressions
				for (field in names(xpath.expr)) {
					v <- XML::xpathSApply(xml, xpath.expr[[field]], XML::xmlValue)
					if (length(v) > 0)
						entry$setField(field, v)
				}

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
