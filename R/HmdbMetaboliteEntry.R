# vi: fdm=marker

# Class declaration {{{1
################################################################

HmdbMetaboliteEntry <- methods::setRefClass("HmdbMetaboliteEntry", contains = "XmlEntry")


# Constructor {{{1
################################################################

HmdbMetaboliteEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addXpathStatement(BIODB.ACCESSION, "/metabolite/accession")
	.self$addXpathStatement(BIODB.KEGG.COMPOUND.ID, "//kegg_id")
	.self$addXpathStatement(BIODB.CHEBI.ID, "//chebi_id")
	.self$addXpathStatement(BIODB.CHEMSPIDER.ID, "//chemspider_id")
	.self$addXpathStatement(BIODB.NCBI.PUBCHEM.COMP.ID, "//pubchem_compound_id")
	.self$addXpathStatement(BIODB.NAME, "/metabolite/name")
	.self$addXpathStatement(BIODB.FORMULA, "/metabolite/chemical_formula")
	.self$addXpathStatement(BIODB.SUPER.CLASS, "//super_class")
	.self$addXpathStatement(BIODB.AVERAGE.MASS, "//average_molecular_weight")
	.self$addXpathStatement(BIODB.MONOISOTOPIC.MASS, "//monisotopic_moleculate_weight")
	.self$addXpathStatement(BIODB.SYNONYMS, "//synonym")
	.self$addXpathStatement(BIODB.COMP.IUPAC.NAME.SYST, "//iupac_name")
	.self$addXpathStatement(BIODB.COMP.IUPAC.NAME.TRAD, "//traditional_iupac")
	.self$addXpathStatement(BIODB.CAS.ID, "//cas_registry_number")
	.self$addXpathStatement(BIODB.SMILES, "//smiles")
	.self$addXpathStatement(BIODB.INCHI, "//inchi")
	.self$addXpathStatement(BIODB.INCHIKEY, "//inchikey")
})

# Before parse content {{{1
################################################################

HmdbMetaboliteEntry$methods( beforeParseContent = function(xml) {
	return(length(XML::getNodeSet(xml, "//error")) == 0)
})

# After parse content {{{1
################################################################

HmdbMetaboliteEntry$methods( afterParseContent = function(xml) {

	# Correct InChIKey
	if (.self$hasField(BIODB.INCHIKEY))
		.self$setFieldValue(BIODB.INCHIKEY, sub('^InChIKey=', '', .self$getFieldValue(BIODB.INCHIKEY), perl = TRUE))
})
