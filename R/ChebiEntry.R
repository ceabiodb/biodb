# vi: fdm=marker

#' @include XmlEntry.R

# Class declaration {{{1
################################################################

ChebiEntry <- methods::setRefClass("ChebiEntry", contains = "XmlEntry")

# Constructor {{{1
################################################################

ChebiEntry$methods( initialize = function(...) {

	callSuper(xml.namespace = c(chebi = "http://www.ebi.ac.uk/webservices/chebi"), ...)

	.self$addParsingExpression(BIODB.SMILES, "//chebi:return/chebi:smiles")
	.self$addParsingExpression(BIODB.INCHI, "//chebi:return/chebi:inchi")
	.self$addParsingExpression(BIODB.INCHIKEY, "//chebi:return/chebi:inchiKey")
	.self$addParsingExpression(BIODB.KEGG.COMPOUND.ID, "//chebi:DatabaseLinks/chebi:type[text()='KEGG COMPOUND accession']/../chebi:data")
	.self$addParsingExpression(BIODB.MASS, "//chebi:mass")
	.self$addParsingExpression(BIODB.MONOISOTOPIC.MASS, "//chebi:monoisotopicMass")
	.self$addParsingExpression(BIODB.CHARGE, "//chebi:charge")
})

# After parse content {{{1
################################################################

ChebiEntry$methods( .afterParseContent = function(xml) {

	# Get accession
	accession <- XML::xpathSApply(xml, "//chebi:return/chebi:chebiId", XML::xmlValue, namespaces = .self$.xml.namespace)
	if (length(accession) > 0) {
		accession <- sub('^CHEBI:([0-9]+)$', '\\1', accession, perl = TRUE)
		.self$setFieldValue(BIODB.ACCESSION, accession)
	}
})
