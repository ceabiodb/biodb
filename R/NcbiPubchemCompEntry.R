# vi: fdm=marker

#' @include NcbiPubchemEntry.R

# Class declaration {{{1
################################################################

NcbiPubchemCompEntry <- methods::setRefClass("NcbiPubchemCompEntry", contains = "NcbiPubchemEntry")

# Constructor {{{1
################################################################

NcbiPubchemCompEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression('ACCESSION', "//PC-CompoundType_id_cid")
	.self$addParsingExpression('INCHI', "//PC-Urn_label[text()='InChI']/../../..//PC-InfoData_value_sval")
	.self$addParsingExpression('INCHIKEY', "//PC-Urn_label[text()='InChIKey']/../../..//PC-InfoData_value_sval")
	.self$addParsingExpression('FORMULA', "//PC-Urn_label[text()='Molecular Formula']/../../..//PC-InfoData_value_sval")
	.self$addParsingExpression('MASS', "//PC-Urn_label[text()='Mass']/../../..//PC-InfoData_value_fval")
	.self$addParsingExpression('MOLECULAR.WEIGHT', "//PC-Urn_label[text()='Molecular Weight']/../../..//PC-InfoData_value_fval")
	.self$addParsingExpression('COMP.IUPAC.NAME.SYST', "//PC-Urn_label[text()='IUPAC Name']/../PC-Urn_name[text()='Systematic']/../../..//PC-InfoData_value_sval")
	.self$addParsingExpression('COMP.IUPAC.NAME.ALLOWED', "//PC-Urn_label[text()='IUPAC Name']/../PC-Urn_name[text()='Allowed']/../../..//PC-InfoData_value_sval")
	.self$addParsingExpression('COMP.IUPAC.NAME.CAS', "//PC-Urn_label[text()='IUPAC Name']/../PC-Urn_name[text()='CAS-like Style']/../../..//PC-InfoData_value_sval")
	.self$addParsingExpression('COMP.IUPAC.NAME.PREF', "//PC-Urn_label[text()='IUPAC Name']/../PC-Urn_name[text()='Preferred']/../../..//PC-InfoData_value_sval")
	.self$addParsingExpression('COMP.IUPAC.NAME.TRAD', "//PC-Urn_label[text()='IUPAC Name']/../PC-Urn_name[text()='Traditional']/../../..//PC-InfoData_value_sval")
	.self$addParsingExpression('LOGP', "//PC-Urn_label[text()='Log P']/../../..//PC-InfoData_value_fval")
	.self$addParsingExpression('SMILES.CANONICAL', "//PC-Urn_label[text()='SMILES']/../PC-Urn_name[text()='Canonical']/../../..//PC-InfoData_value_sval")
	.self$addParsingExpression('SMILES.ISOMERIC', "//PC-Urn_label[text()='SMILES']/../PC-Urn_name[text()='Isomeric']/../../..//PC-InfoData_value_sval")
})
