# vi: fdm=marker

#' @include NcbiPubchemEntry.R

# Class declaration {{{1
################################################################

NcbiPubchemCompEntry <- methods::setRefClass("NcbiPubchemCompEntry", contains = "NcbiPubchemEntry")

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
	.self$addParsingExpression(BIODB.COMP.IUPAC.NAME.ALLOWED, "//PC-Urn_label[text()='IUPAC Name']/../PC-Urn_name[text()='Allowed']/../../..//PC-InfoData_value_sval")
	.self$addParsingExpression(BIODB.COMP.IUPAC.NAME.CAS, "//PC-Urn_label[text()='IUPAC Name']/../PC-Urn_name[text()='CAS-like Style']/../../..//PC-InfoData_value_sval")
	.self$addParsingExpression(BIODB.COMP.IUPAC.NAME.PREF, "//PC-Urn_label[text()='IUPAC Name']/../PC-Urn_name[text()='Preferred']/../../..//PC-InfoData_value_sval")
	.self$addParsingExpression(BIODB.COMP.IUPAC.NAME.TRAD, "//PC-Urn_label[text()='IUPAC Name']/../PC-Urn_name[text()='Traditional']/../../..//PC-InfoData_value_sval")
	.self$addParsingExpression(BIODB.COMP.LOGP, "//PC-Urn_label[text()='Log P']/../../..//PC-InfoData_value_fval")
	.self$addParsingExpression(BIODB.SMILES.CANONICAL, "//PC-Urn_label[text()='SMILES']/../PC-Urn_name[text()='Canonical']/../../..//PC-InfoData_value_sval")
	.self$addParsingExpression(BIODB.SMILES.ISOMERIC, "//PC-Urn_label[text()='SMILES']/../PC-Urn_name[text()='Isomeric']/../../..//PC-InfoData_value_sval")
})
