# vi: fdm=marker

#' @include JsonEntry.R

# Class declaration {{{1
################################################################

PeakforestCompoundEntry <- methods::setRefClass("PeakforestCompoundEntry", contains = "JsonEntry")

# Constructor {{{1
################################################################

PeakforestCompoundEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression('ACCESSION', "id")
	.self$addParsingExpression('ncbi.pubchem.comp.id', "PubChemCID")
	.self$addParsingExpression('chebi.id', "ChEBI")
	.self$addParsingExpression('hmdb.metabolites.id', "HMDB")
	.self$addParsingExpression('kegg.compound.id', "KEGG")
	.self$addParsingExpression('FORMULA', "formula")
	.self$addParsingExpression('SMILES', "canSmiles")
	.self$addParsingExpression('AVERAGE.MASS', "averageMass")
	.self$addParsingExpression('MONOISOTOPIC.MASS', "monoisotopicMass")
	.self$addParsingExpression('inchi', "inChI")
	.self$addParsingExpression('inchikey', "inChIKey")
	.self$addParsingExpression('name', "mainName")
})

# Parse fields after {{{1
################################################################

PeakforestCompoundEntry$methods( .parseFieldsAfter = function(parsed.content) {

	# HMDB null
	if (.self$hasField('hmdb.metabolites.id')) {
		v <- .self$getFieldValue('hmdb.metabolites.id')
		v <- v[v != 'HMDBnull']
		if (length(v) > 0)
			.self$setFieldValue('hmdb.metabolites.id', v)
		else
			.self$removeField('hmdb.metabolites.id')
	}

	# ChEBI IDs
	if (.self$hasField('chebi.id')) {
		v <- .self$getFieldValue('chebi.id')
		v <- sub('^CHEBI:', '', v)
		.self$setFieldValue('chebi.id', v)
	}
})
