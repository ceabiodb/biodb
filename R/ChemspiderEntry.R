# vi: fdm=marker

# Class declaration {{{1
################################################################

ChemspiderEntry <- methods::setRefClass("ChemspiderEntry", contains = "XmlEntry")

# Constructor {{{1
################################################################

ChemspiderEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression(BIODB.ACCESSION, "//CSID")
	.self$addParsingExpression(BIODB.FORMULA, "//MF")
	.self$addParsingExpression(BIODB.NAME, "//CommonName")
	.self$addParsingExpression(BIODB.AVERAGE.MASS, "//AverageMass")
	.self$addParsingExpression(BIODB.MONOISOTOPIC.MASS, "//MonoisotopicMass")
	.self$addParsingExpression(BIODB.NOMINAL.MASS, "//NominalMass")
	.self$addParsingExpression(BIODB.MOLECULAR.WEIGHT, "//MolecularWeight")
	.self$addParsingExpression(BIODB.INCHI, "//InChI")
	.self$addParsingExpression(BIODB.INCHIKEY, "//InChIKey")
	.self$addParsingExpression(BIODB.SMILES, "//SMILES")
})
