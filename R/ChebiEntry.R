# vi: fdm=marker

#' @include XmlEntry.R

# Class declaration {{{1
################################################################

ChebiEntry <- methods::setRefClass("ChebiEntry", contains = "XmlEntry")

# Constructor {{{1
################################################################

ChebiEntry$methods( initialize = function(...) {

	callSuper(namespace = c(chebi = .self$getParent()$getDbInfo()$getXmlNs()), ...)

#	.self$addParsingExpression('accession', "replace(//chebi:return/chebi:chebiId,'^CHEBI:([0-9]+)$','$1')")
#	.self$addParsingExpression('accession', "substring(//chebi:return/chebi:chebiId,7)")
	.self$addParsingExpression('accession', "substring-after(//chebi:return/chebi:chebiId,'CHEBI:')")
	.self$addParsingExpression('SMILES', "//chebi:return/chebi:smiles")
	.self$addParsingExpression('INCHI', "//chebi:return/chebi:inchi")
	.self$addParsingExpression('INCHIKEY', "//chebi:return/chebi:inchiKey")
	.self$addParsingExpression('kegg.compound.id', "//chebi:DatabaseLinks/chebi:type[text()='KEGG COMPOUND accession']/../chebi:data")
	.self$addParsingExpression('MASS', "//chebi:mass")
	.self$addParsingExpression('MONOISOTOPIC.MASS', "//chebi:monoisotopicMass")
	.self$addParsingExpression('CHARGE', "//chebi:charge")
	.self$addParsingExpression('name', c("//chebi:chebiAsciiName", "//chebi:Synonyms/chebi:data"))
	.self$addParsingExpression('formula', c("//chebi:Formulae/chebi:source[text()='ChEBI']/../chebi:data", "(//chebi:Formulae/chebi:data)[1]"))
})

# Parse fields after {{{1
################################################################

ChebiEntry$methods( .parseFieldsAfter = function(parsed.content) {

#	# Check that we do not have a better formula (there may be several formulae defined).
#	formulae <- XML::xpathSApply(parsed.content, "//chebi:Formulae/chebi:data", XML::xmlValue, namespaces = .self$.namespace)
#	if (length(formulae) > 1) {
#		# Try to get the ChEBI formula
#		formula <- XML::xpathSApply(parsed.content, "//chebi:Formulae/chebi:source[text()='ChEBI']/../chebi:data", XML::xmlValue, namespaces = .self$.namespace)
#		if (length(formula) > 0)
#			.self$setFieldValue('formula', formula)
#	}

	# Get synonyms
#	synonyms <- XML::xpathSApply(parsed.content, "//chebi:Synonyms/chebi:data", XML::xmlValue, namespaces = .self$.namespace)
#	if (length(synonyms) > 0)
#		.self$appendFieldValue('name', synonyms)
})
