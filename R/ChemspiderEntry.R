# vi: fdm=marker

# Class declaration {{{1
################################################################

ChemspiderEntry <- methods::setRefClass("ChemspiderEntry", contains = "XmlEntry")

# Constructor {{{1
################################################################

ChemspiderEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression('ACCESSION', "//CSID")
	.self$addParsingExpression('FORMULA', "//MF")
	.self$addParsingExpression('NAME', "//CommonName")
	.self$addParsingExpression('AVERAGE.MASS', "//AverageMass")
	.self$addParsingExpression('MONOISOTOPIC.MASS', "//MonoisotopicMass")
	.self$addParsingExpression('NOMINAL.MASS', "//NominalMass")
	.self$addParsingExpression('MOLECULAR.WEIGHT', "//MolecularWeight")
	.self$addParsingExpression('INCHI', "//InChI")
	.self$addParsingExpression('INCHIKEY', "//InChIKey")
	.self$addParsingExpression('SMILES', "//SMILES")
})
