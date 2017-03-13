# vi: fdm=marker

#' @include JsonEntry.R

# Class declaration {{{1
################################################################

PeakforestCompoundEntry <- methods::setRefClass("PeakforestCompoundEntry", contains = "JsonEntry")

# Constructor {{{1
################################################################

PeakforestCompoundEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression(BIODB.ACCESSION, "id")
	.self$addParsingExpression(BIODB.NCBI.PUBCHEM.COMP.ID, "PubChemCID")
	.self$addParsingExpression(BIODB.CHEBI.ID, "ChEBI")
	.self$addParsingExpression(BIODB.HMDB.METABOLITE.ID, "HMDB")
	.self$addParsingExpression(BIODB.KEGG.COMPOUND.ID, "KEGG")
	.self$addParsingExpression(BIODB.FORMULA, "formula")
	.self$addParsingExpression(BIODB.SMILES, "canSmiles")
	.self$addParsingExpression(BIODB.AVERAGE.MASS, "averageMass")
	.self$addParsingExpression(BIODB.MONOISOTOPIC.MASS, "monoisotopicMass")
	.self$addParsingExpression(BIODB.INCHI, "inChI")
	.self$addParsingExpression(BIODB.INCHIKEY, "inchiIKey")
	.self$addParsingExpression(BIODB.NAME, "mainName")
})

# Parse fields after {{{1
################################################################

PeakforestCompoundEntry$methods( .parseFieldsAfter = function(parsed.content) {

	# HMDB null
	if (.self$hasField(BIODB.HMDB.METABOLITE.ID)) {
		v <- .self$getFieldValue(BIODB.HMDB.METABOLITE.ID)
		v <- v[v != 'HMDBnull']
		.self$setFieldValue(BIODB.HMDB.METABOLITE.ID, v)
	}

	# ChEBI IDs
	if (.self$hasField(BIODB.CHEBI.ID)) {
		v <- .self$getFieldValue(BIODB.CHEBI.ID)
		v <- sub('^CHEBI:', '', v)
		.self$setFieldValue(BIODB.CHEBI.ID, v)
	}
})
