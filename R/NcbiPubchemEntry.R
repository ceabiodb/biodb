#####################
# CLASS DECLARATION #
#####################

NcbiPubchemEntry <- methods::setRefClass("NcbiPubchemEntry", contains = "BiodbEntry")
NcbiPubchemSubstance <- methods::setRefClass("NcbiPubchemSubstance", contains = "BiodbEntry")

#####################
# SUBSTANCE FACTORY #
#####################

createNcbiPubchemSubstanceFromXml <- function(biodb, contents, drop = TRUE) {

	entries <- list()

	# Define xpath expressions
	xpath.expr <- character()
	xpath.expr[[BIODB.ACCESSION]] <- "//PC-ID_id"
	#xpath.expr[[BIODB.PUBCHEM.COMP.ID]] <- "//PC-CompoundType_id_cid" --> Apparently that can be more than one CID for a substance.

	for (content in contents) {

		# Create instance
		entry <- NcbiPubchemEntry$new(biodb = biodb)

		if ( ! is.null(content) && ! is.na(content)) {

			# Parse XML
			xml <-  XML::xmlInternalTreeParse(content, asText = TRUE)

			# Unknown entry
			fault <- XML::xpathSApply(xml, "/Fault", XML::xmlValue)
			if (length(fault) == 0) {

				# Test generic xpath expressions
				for (field in names(xpath.expr)) {
					v <- XML::xpathSApply(xml, xpath.expr[[field]], XML::xmlValue)
					if (length(v) > 0)
						entry$setFieldValue(field, v)
				}
			}
		}

		entries <- c(entries, entry)
	}

	# Replace elements with no accession id by NULL
	entries <- lapply(entries, function(x) if (is.na(x$getFieldValue(BIODB.ACCESSION))) NULL else x)

	# If the input was a single element, then output a single object
	if (drop && length(contents) == 1)
		entries <- entries[[1]]

	return(entries)
}

####################
# COMPOUND FACTORY #
####################

createNcbiPubchemEntryFromXml <- function(biodb, contents, drop = TRUE) {

	entries <- list()

	# Define xpath expressions
	xpath.expr <- character()
	xpath.expr[[BIODB.ACCESSION]] <- "//PC-CompoundType_id_cid"
	xpath.expr[[BIODB.INCHI]] <- "//PC-Urn_label[text()='InChI']/../../..//PC-InfoData_value_sval"
	xpath.expr[[BIODB.INCHIKEY]] <- "//PC-Urn_label[text()='InChIKey']/../../..//PC-InfoData_value_sval"
	xpath.expr[[BIODB.FORMULA]] <- "//PC-Urn_label[text()='Molecular Formula']/../../..//PC-InfoData_value_sval"
	xpath.expr[[BIODB.MASS]] <- "//PC-Urn_label[text()='Mass']/../../..//PC-InfoData_value_fval"
	xpath.expr[[BIODB.MOLECULAR.WEIGHT]] <- "//PC-Urn_label[text()='Molecular Weight']/../../..//PC-InfoData_value_fval"
	xpath.expr[[BIODB.COMP.IUPAC.NAME.SYST]] <- "//PC-Urn_label[text()='IUPAC Name']/../PC-Urn_name[text()='Systematic']/../../..//PC-InfoData_value_sval"

	for (content in contents) {

		# Create instance
		entry <- NcbiPubchemEntry$new(biodb = biodb)

		if ( ! is.null(content) && ! is.na(content)) {

			# Parse XML
			xml <-  XML::xmlInternalTreeParse(content, asText = TRUE)

			# Unknown entry
			fault <- XML::xpathSApply(xml, "/Fault", XML::xmlValue)
			if (length(fault) == 0) {

				# Test generic xpath expressions
				for (field in names(xpath.expr)) {
					v <- XML::xpathSApply(xml, xpath.expr[[field]], XML::xmlValue)
					if (length(v) > 0)
						entry$setFieldValue(field, v)
				}
			}
		}

		entries <- c(entries, entry)
	}

	# Replace elements with no accession id by NULL
	entries <- lapply(entries, function(x) if (is.na(x$getFieldValue(BIODB.ACCESSION))) NULL else x)

	# If the input was a single element, then output a single object
	if (drop && length(contents) == 1)
		entries <- entries[[1]]

	return(entries)
}
