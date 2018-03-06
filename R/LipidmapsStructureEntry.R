# vi: fdm=marker

#' @include CsvEntry.R

# Class declaration {{{1
################################################################

LipidmapsStructureEntry <- methods::setRefClass("LipidmapsStructureEntry", contains = 'CsvEntry')

# Constructor {{{1
################################################################

LipidmapsStructureEntry$methods( initialize = function(...) {

	callSuper(na.strings = '-', ...)

	.self$addParsingExpression('NAME', 'COMMON_NAME')
	.self$addParsingExpression('ACCESSION', 'LM_ID')
	.self$addParsingExpression('kegg.compound.id', 'KEGG_ID')
	.self$addParsingExpression('hmdb.metabolites.id', 'HMDBID')
	.self$addParsingExpression('chebi.id', 'CHEBI_ID')
	.self$addParsingExpression('ncbi.pubchem.comp.id', 'PUBCHEM_COMPOUND_ID')
	.self$addParsingExpression('exact.mass', 'MASS')
	.self$addParsingExpression('FORMULA', 'FORMULA')
	.self$addParsingExpression('COMP.IUPAC.NAME.SYST', 'SYSTEMATIC_NAME')
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
	if (.self$hasField('SYNONYMS')) {
		v <- strsplit(.self$getFieldValue('SYNONYMS'), ';')[[1]]
		v <- sub('^ +', '', v, perl = TRUE)
		v <- sub(' +$', '', v, perl = TRUE)
		.self$setFieldValue('SYNONYMS', v)
	}

	# Synonyms
	if ('SYNONYMS' %in% names(parsed.content)) {
		v <- parsed.content[['SYNONYMS']]
		if ( ! is.na(v)) {
			v <- strsplit(v, ' *; *')[[1]]
			.self$appendFieldValue('name', v)
		}
	}
})
