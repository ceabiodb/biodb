# vi: fdm=marker

# Class declaration {{{1
################################################################

ChemspiderEntry <- methods::setRefClass("ChemspiderEntry", contains = "XmlEntry")

# Constructor {{{1
################################################################

ChemspiderEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addXpathStatement(BIODB.ACCESSION, "//CSID")
	.self$addXpathStatement(BIODB.FORMULA, "//MF")
	.self$addXpathStatement(BIODB.NAME, "//CommonName")
	.self$addXpathStatement(BIODB.AVERAGE.MASS, "//AverageMass")
	.self$addXpathStatement(BIODB.MONOISOTOPIC.MASS, "//MonoisotopicMass")
	.self$addXpathStatement(BIODB.NOMINAL.MASS, "//NominalMass")
	.self$addXpathStatement(BIODB.MOLECULAR.WEIGHT, "//MolecularWeight")
	.self$addXpathStatement(BIODB.INCHI, "//InChI")
	.self$addXpathStatement(BIODB.INCHIKEY, "//InChIKey")
	.self$addXpathStatement(BIODB.SMILES, "//SMILES")
})
