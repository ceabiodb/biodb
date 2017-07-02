# vi: fdm=marker

#' @include XmlEntry.R

# Class declaration {{{1
################################################################

UniprotEntry <- methods::setRefClass("UniprotEntry", contains = "XmlEntry")

# Constructor {{{1
################################################################

UniprotEntry$methods( initialize = function(...) {

	callSuper(namespace = c(uniprot = "http://uniprot.org/uniprot"), ...)

	.self$addParsingExpression(BIODB.NAME, "/uniprot:uniprot/uniprot:entry/uniprot:name")
	.self$addParsingExpression(BIODB.GENE.SYMBOLS, "//uniprot:gene/uniprot:name")
	.self$addParsingExpression(BIODB.FULLNAMES, "//uniprot:protein//uniprot:fullName")
	.self$addParsingExpression(BIODB.SEQUENCE, "//uniprot:entry/uniprot:sequence")
	.self$addParsingExpression(BIODB.ACCESSION, "//uniprot:accession[1]")
	.self$addParsingExpression(BIODB.KEGG.COMPOUND.ID, list(path = "//uniprot:dbReference[@type='KEGG']", attr = 'id'))
	.self$addParsingExpression(BIODB.NCBI.GENE.ID, list(path = "//uniprot:dbReference[@type='GeneID']", attr = 'id'))
	.self$addParsingExpression(BIODB.EXPASY.ENZYME.ID, list(path = "//uniprot:dbReference[@type='EC']", attr = 'id'))
	.self$addParsingExpression(BIODB.MASS, list(path = "//uniprot:entry/uniprot:sequence", attr = 'mass'))
	.self$addParsingExpression(BIODB.LENGTH, list(path = "//uniprot:entry/uniprot:sequence", attr = 'length'))
})

# Is content correct {{{1
################################################################

UniprotEntry$methods( .isContentCorrect = function(content) {
	return( ! grepl("^<!DOCTYPE html ", content, perl = TRUE))
})

# Parse fields after {{{1
################################################################

UniprotEntry$methods( .parseFieldsAfter = function(parsed.content) {

	# Remove new lines from sequence string
	if (.self$hasField(BIODB.SEQUENCE))
		.self$setField(BIODB.SEQUENCE, gsub("\\n", "", .self$getFieldValue(BIODB.SEQUENCE)))
})
