# vi: fdm=marker

# Class declaration {{{1
################################################################

ChebiEntry <- methods::setRefClass("ChebiEntry", contains = "BiodbEntry")

# Parse content {{{1
################################################################

ChebiEntry$methods( parseContent = function(content) {

	# Define xpath expressions
	xpath.expr <- character()
	xpath.expr[[BIODB.SMILES]] <- "//chebi:return/chebi:smiles"
	xpath.expr[[BIODB.INCHI]] <- "//chebi:return/chebi:inchi"
	xpath.expr[[BIODB.INCHIKEY]] <- "//chebi:return/chebi:inchiKey"
	xpath.expr[[BIODB.KEGG.COMPOUND.ID]] <- "//chebi:DatabaseLinks/chebi:type[text()='KEGG COMPOUND accession']/../chebi:data"
	xpath.expr[[BIODB.MASS]] <- "//chebi:mass"
	xpath.expr[[BIODB.MONOISOTOPIC.MASS]] <- "//chebi:monoisotopicMass"
	xpath.expr[[BIODB.CHARGE]] <- "//chebi:charge"

	ns <- c(chebi = "http://www.ebi.ac.uk/webservices/chebi")
	
	# Parse XMLV
	xml <-  XML::xmlInternalTreeParse(content, asText = TRUE)

	# Test generic xpath expressions
	for (field in names(xpath.expr)) {
		v <- XML::xpathSApply(xml, xpath.expr[[field]], XML::xmlValue, namespaces = ns)
		if (length(v) > 0)
			.self$setFieldValue(field, v)
	}

	# Get accession
	accession <- XML::xpathSApply(xml, "//chebi:return/chebi:chebiId", XML::xmlValue, namespaces = ns)
	if (length(accession) > 0) {
		accession <- sub('^CHEBI:([0-9]+)$', '\\1', accession, perl = TRUE)
		.self$setFieldValue(BIODB.ACCESSION, accession)
	}
})
