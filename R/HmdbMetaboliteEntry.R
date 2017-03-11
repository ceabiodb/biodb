# vi: fdm=marker

# Class declaration {{{1
################################################################

HmdbMetaboliteEntry <- methods::setRefClass("HmdbMetaboliteEntry", contains = "XmlEntry")


# Constructor {{{1
################################################################

HmdbMetaboliteEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression(BIODB.ACCESSION, "/metabolite/accession")
	.self$addParsingExpression(BIODB.KEGG.COMPOUND.ID, "//kegg_id")
	.self$addParsingExpression(BIODB.CHEBI.ID, "//chebi_id")
	.self$addParsingExpression(BIODB.CHEMSPIDER.ID, "//chemspider_id")
	.self$addParsingExpression(BIODB.NCBI.PUBCHEM.COMP.ID, "//pubchem_compound_id")
	.self$addParsingExpression(BIODB.NAME, "/metabolite/name")
	.self$addParsingExpression(BIODB.FORMULA, "/metabolite/chemical_formula")
	.self$addParsingExpression(BIODB.SUPER.CLASS, "//super_class")
	.self$addParsingExpression(BIODB.AVERAGE.MASS, "//average_molecular_weight")
	.self$addParsingExpression(BIODB.MONOISOTOPIC.MASS, "//monisotopic_moleculate_weight")
	.self$addParsingExpression(BIODB.SYNONYMS, "//synonym")
	.self$addParsingExpression(BIODB.COMP.IUPAC.NAME.SYST, "//iupac_name")
	.self$addParsingExpression(BIODB.COMP.IUPAC.NAME.TRAD, "//traditional_iupac")
	.self$addParsingExpression(BIODB.CAS.ID, "//cas_registry_number")
	.self$addParsingExpression(BIODB.SMILES, "//smiles")
	.self$addParsingExpression(BIODB.INCHI, "//inchi")
	.self$addParsingExpression(BIODB.INCHIKEY, "//inchikey")
})

# Is parsed content correct {{{1
################################################################

HmdbMetaboliteEntry$methods( .isParsedContentCorrect = function(parsed.content) {
	return(length(XML::getNodeSet(parsed.content, "//error")) == 0)
})

# Parse fields after {{{1
################################################################

HmdbMetaboliteEntry$methods( .parseFieldsAfter = function(parsed.content) {

	# Correct InChIKey
	if (.self$hasField(BIODB.INCHIKEY))
		.self$setFieldValue(BIODB.INCHIKEY, sub('^InChIKey=', '', .self$getFieldValue(BIODB.INCHIKEY), perl = TRUE))
})
