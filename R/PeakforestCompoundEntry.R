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
	.self$addParsingExpression('ncbi.pubchem.comp.id', "PubChemCID")
	.self$addParsingExpression('chebi.id', "ChEBI")
	.self$addParsingExpression('hmdb.metabolite.id', "HMDB")
	.self$addParsingExpression('kegg.compound.id', "KEGG")
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
	if (.self$hasField('hmdb.metabolite.id')) {
		v <- .self$getFieldValue('hmdb.metabolite.id')
		v <- v[v != 'HMDBnull']
		if (length(v) > 0)
			.self$setFieldValue('hmdb.metabolite.id', v)
		else
			.self$removeField('hmdb.metabolite.id')
	}

	# ChEBI IDs
	if (.self$hasField('chebi.id')) {
		v <- .self$getFieldValue('chebi.id')
		v <- sub('^CHEBI:', '', v)
		.self$setFieldValue('chebi.id', v)
	}
})
