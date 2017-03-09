# vi: fdm=marker

# Class declaration {{{1
################################################################

ChebiEntry <- methods::setRefClass("ChebiEntry", contains = "XmlEntry")

# Constructor {{{1
################################################################

ChebiEntry$methods( initialize = function(...) {

	callSuper(xml.namespace = c(chebi = "http://www.ebi.ac.uk/webservices/chebi"), ...)

	.self$addXpathStatement(BIODB.SMILES, "//chebi:return/chebi:smiles")
	.self$addXpathStatement(BIODB.INCHI, "//chebi:return/chebi:inchi")
	.self$addXpathStatement(BIODB.INCHIKEY, "//chebi:return/chebi:inchiKey")
	.self$addXpathStatement(BIODB.KEGG.COMPOUND.ID, "//chebi:DatabaseLinks/chebi:type[text()='KEGG COMPOUND accession']/../chebi:data")
	.self$addXpathStatement(BIODB.MASS, "//chebi:mass")
	.self$addXpathStatement(BIODB.MONOISOTOPIC.MASS, "//chebi:monoisotopicMass")
	.self$addXpathStatement(BIODB.CHARGE, "//chebi:charge")
})

# Run custom xpath statements {{{1
################################################################

ChebiEntry$methods( runCustomXpathStatements = function(xml) {

	# Get accession
	accession <- XML::xpathSApply(xml, "//chebi:return/chebi:chebiId", XML::xmlValue, namespaces = .self$.xml.namespace)
	if (length(accession) > 0) {
		accession <- sub('^CHEBI:([0-9]+)$', '\\1', accession, perl = TRUE)
		.self$setFieldValue(BIODB.ACCESSION, accession)
	}
})
