# vi: fdm=marker

#' @include XmlEntry.R

# Class declaration {{{1
################################################################

UniprotEntry <- methods::setRefClass("UniprotEntry", contains = "XmlEntry")

# Constructor {{{1
################################################################

UniprotEntry$methods( initialize = function(...) {

	callSuper(namespace = c(uniprot = "http://uniprot.org/uniprot"), ...)

	.self$addParsingExpression('NAME', "/uniprot:uniprot/uniprot:entry/uniprot:name")
	.self$addParsingExpression('GENE.SYMBOLS', "//uniprot:gene/uniprot:name")
	.self$addParsingExpression('FULLNAMES', "//uniprot:protein//uniprot:fullName")
	.self$addParsingExpression('SEQUENCE', "//uniprot:entry/uniprot:sequence")
	.self$addParsingExpression('ACCESSION', "//uniprot:accession[1]")
	.self$addParsingExpression('kegg.compound.id', list(path = "//uniprot:dbReference[@type='KEGG']", attr = 'id'))
	.self$addParsingExpression('ncbi.gene.id', list(path = "//uniprot:dbReference[@type='GeneID']", attr = 'id'))
	.self$addParsingExpression('expasy.enzyme.id', list(path = "//uniprot:dbReference[@type='EC']", attr = 'id'))
	.self$addParsingExpression('MASS', list(path = "//uniprot:entry/uniprot:sequence", attr = 'mass'))
	.self$addParsingExpression('LENGTH', list(path = "//uniprot:entry/uniprot:sequence", attr = 'length'))
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
	if (.self$hasField('SEQUENCE'))
		.self$setField('SEQUENCE', gsub("\\n", "", .self$getFieldValue('SEQUENCE')))
})
