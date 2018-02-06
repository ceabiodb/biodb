# vi: fdm=marker

#' @include XmlEntry.R

# Class declaration {{{1
################################################################

HmdbMetabolitesEntry <- methods::setRefClass("HmdbMetabolitesEntry", contains = "XmlEntry")

# Constructor {{{1
################################################################

HmdbMetabolitesEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression('ACCESSION', "/metabolite/accession")
	.self$addParsingExpression('kegg.compound.id', "//kegg_id")
	.self$addParsingExpression('chebi.id', "//chebi_id")
	.self$addParsingExpression('chemspider.id', "//chemspider_id")
	.self$addParsingExpression('ncbi.pubchem.comp.id', "//pubchem_compound_id")
	.self$addParsingExpression('NAME', "/metabolite/name")
	.self$addParsingExpression('FORMULA', "/metabolite/chemical_formula")
	.self$addParsingExpression('SUPER.CLASS', "//super_class")
	.self$addParsingExpression('AVERAGE.MASS', "//average_molecular_weight")
	.self$addParsingExpression('MONOISOTOPIC.MASS', "//monisotopic_molecular_weight")
	.self$addParsingExpression('COMP.IUPAC.NAME.SYST', "//iupac_name")
	.self$addParsingExpression('COMP.IUPAC.NAME.TRAD', "//traditional_iupac")
	.self$addParsingExpression('CAS.ID', "//cas_registry_number")
	.self$addParsingExpression('SMILES', "//smiles")
	.self$addParsingExpression('INCHI', "//inchi")
	.self$addParsingExpression('INCHIKEY', "//inchikey")
})

# Is parsed content correct {{{1
################################################################

HmdbMetabolitesEntry$methods( .isParsedContentCorrect = function(parsed.content) {
	return(length(XML::getNodeSet(parsed.content, "//error")) == 0)
})

# Parse fields after {{{1
################################################################

HmdbMetabolitesEntry$methods( .parseFieldsAfter = function(parsed.content) {

	# Correct InChIKey
	if (.self$hasField('INCHIKEY'))
		.self$setFieldValue('INCHIKEY', sub('^InChIKey=', '', .self$getFieldValue('INCHIKEY'), perl = TRUE))

	# Synonyms
	synonyms <- XML::xpathSApply(parsed.content, "//synonym", XML::xmlValue)
	if (length(synonyms) > 0)
		.self$appendFieldValue('name', synonyms)
})
