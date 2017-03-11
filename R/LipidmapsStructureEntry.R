# vi: fdm=marker

#' @include CsvEntry.R

# Class declaration {{{1
################################################################

LipidmapsStructureEntry <- methods::setRefClass("LipidmapsStructureEntry", contains = 'CsvEntry')

# Constructor {{{1
################################################################

LipidmapsStructureEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression(BIODB.NAME, 'COMMON_NAME')
	.self$addParsingExpression(BIODB.ACCESSION, 'LM_ID')
	.self$addParsingExpression(BIODB.KEGG.COMPOUND.ID, 'KEGG_ID')
	.self$addParsingExpression(BIODB.HMDB.METABOLITE.ID, 'HMDBID')
	.self$addParsingExpression(BIODB.MASS, 'MASS')
	.self$addParsingExpression(BIODB.FORMULA, 'FORMULA')
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
	if (parsed.content[['SYNONYMS']] != '-') {
		# TODO
	}
})
