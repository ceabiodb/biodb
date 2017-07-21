# vi: fdm=marker

#' @include XmlEntry.R

# Class declaration {{{1
################################################################

ChebiEntry <- methods::setRefClass("ChebiEntry", contains = "XmlEntry")

# Constructor {{{1
################################################################

ChebiEntry$methods( initialize = function(...) {

	callSuper(namespace = c(chebi = "http://www.ebi.ac.uk/webservices/chebi"), ...)

	.self$addParsingExpression('SMILES', "//chebi:return/chebi:smiles")
	.self$addParsingExpression('INCHI', "//chebi:return/chebi:inchi")
	.self$addParsingExpression('INCHIKEY', "//chebi:return/chebi:inchiKey")
	.self$addParsingExpression('kegg.compound.id', "//chebi:DatabaseLinks/chebi:type[text()='KEGG COMPOUND accession']/../chebi:data")
	.self$addParsingExpression('MASS', "//chebi:mass")
	.self$addParsingExpression('MONOISOTOPIC.MASS', "//chebi:monoisotopicMass")
	.self$addParsingExpression('CHARGE', "//chebi:charge")
})

# Parse fields after {{{1
################################################################

ChebiEntry$methods( .parseFieldsAfter = function(parsed.content) {

	# Get accession
	accession <- XML::xpathSApply(parsed.content, "//chebi:return/chebi:chebiId", XML::xmlValue, namespaces = .self$.namespace)
	if (length(accession) > 0) {
		accession <- sub('^CHEBI:([0-9]+)$', '\\1', accession, perl = TRUE)
		.self$setFieldValue('ACCESSION', accession)
	}
})
