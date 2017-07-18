# vi: fdm=marker

#' @include CsvEntry.R

# Class declaration {{{1
################################################################

LipidmapsStructureEntry <- methods::setRefClass("LipidmapsStructureEntry", contains = 'CsvEntry')

# Constructor {{{1
################################################################

LipidmapsStructureEntry$methods( initialize = function(...) {

	callSuper(na.strings = '-', ...)

	.self$addParsingExpression(BIODB.NAME, 'COMMON_NAME')
	.self$addParsingExpression(BIODB.SYNONYMS, 'SYNONYMS')
	.self$addParsingExpression(BIODB.ACCESSION, 'LM_ID')
	.self$addParsingExpression('kegg.compound.id', 'KEGG_ID')
	.self$addParsingExpression('hmdb.metabolite.id', 'HMDBID')
	.self$addParsingExpression('chebi.id', 'CHEBI_ID')
	.self$addParsingExpression('ncbi.pubchem.comp.id', 'PUBCHEM_COMPOUND_ID')
	.self$addParsingExpression(BIODB.MASS, 'MASS')
	.self$addParsingExpression(BIODB.FORMULA, 'FORMULA')
	.self$addParsingExpression(BIODB.COMP.IUPAC.NAME.SYST, 'SYSTEMATIC_NAME')
})

# Is content correct {{{1
################################################################

LipidmapsStructureEntry$methods( .isContentCorrect = function(content) {
	return( ! grepl("No record found", content))
})

# Parse fields after {{{1
################################################################

LipidmapsStructureEntry$methods( .parseFieldsAfter = function(parsed.content) {

	# Set synonyms 
	if (.self$hasField(BIODB.SYNONYMS)) {
		v <- strsplit(.self$getFieldValue(BIODB.SYNONYMS), ';')[[1]]
		v <- sub('^ +', '', v, perl = TRUE)
		v <- sub(' +$', '', v, perl = TRUE)
		.self$setFieldValue(BIODB.SYNONYMS, v)
	}
})
