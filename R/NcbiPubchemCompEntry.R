# vi: fdm=marker

#' @include XmlEntry.R

# Class declaration {{{1
################################################################

NcbiPubchemCompEntry <- methods::setRefClass("NcbiPubchemCompEntry", contains = "XmlEntry")

# Constructor {{{1
################################################################

NcbiPubchemCompEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression(BIODB.ACCESSION, "//PC-CompoundType_id_cid")
	.self$addParsingExpression(BIODB.INCHI, "//PC-Urn_label[text()='InChI']/../../..//PC-InfoData_value_sval")
	.self$addParsingExpression(BIODB.INCHIKEY, "//PC-Urn_label[text()='InChIKey']/../../..//PC-InfoData_value_sval")
	.self$addParsingExpression(BIODB.FORMULA, "//PC-Urn_label[text()='Molecular Formula']/../../..//PC-InfoData_value_sval")
	.self$addParsingExpression(BIODB.MASS, "//PC-Urn_label[text()='Mass']/../../..//PC-InfoData_value_fval")
	.self$addParsingExpression(BIODB.MOLECULAR.WEIGHT, "//PC-Urn_label[text()='Molecular Weight']/../../..//PC-InfoData_value_fval")
	.self$addParsingExpression(BIODB.COMP.IUPAC.NAME.SYST, "//PC-Urn_label[text()='IUPAC Name']/../PC-Urn_name[text()='Systematic']/../../..//PC-InfoData_value_sval")
})

# Is parsed content correct {{{1
################################################################

NcbiPubchemCompEntry$methods( .isParsedContentCorrect = function(parsed.content) {
	fault <- XML::xpathSApply(parsed.content, "/Fault", XML::xmlValue)
	return(length(fault) == 0)
})
