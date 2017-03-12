# vi: fdm=marker

#' @include XmlEntry.R

# Class declaration {{{1
################################################################

ChebiEntry <- methods::setRefClass("ChebiEntry", contains = "XmlEntry")

# Constructor {{{1
################################################################

ChebiEntry$methods( initialize = function(...) {

	callSuper(namespace = c(chebi = "http://www.ebi.ac.uk/webservices/chebi"), ...)

	.self$addParsingExpression(BIODB.SMILES, "//chebi:return/chebi:smiles")
	.self$addParsingExpression(BIODB.INCHI, "//chebi:return/chebi:inchi")
	.self$addParsingExpression(BIODB.INCHIKEY, "//chebi:return/chebi:inchiKey")
	.self$addParsingExpression(BIODB.KEGG.COMPOUND.ID, "//chebi:DatabaseLinks/chebi:type[text()='KEGG COMPOUND accession']/../chebi:data")
	.self$addParsingExpression(BIODB.MASS, "//chebi:mass")
	.self$addParsingExpression(BIODB.MONOISOTOPIC.MASS, "//chebi:monoisotopicMass")
	.self$addParsingExpression(BIODB.CHARGE, "//chebi:charge")
})

# Parse fields after {{{1
################################################################

ChebiEntry$methods( .parseFieldsAfter = function(parsed.content) {

	# Get accession
	accession <- XML::xpathSApply(parsed.content, "//chebi:return/chebi:chebiId", XML::xmlValue, namespaces = .self$.namespace)
	if (length(accession) > 0) {
		accession <- sub('^CHEBI:([0-9]+)$', '\\1', accession, perl = TRUE)
		.self$setFieldValue(BIODB.ACCESSION, accession)
	}
})
