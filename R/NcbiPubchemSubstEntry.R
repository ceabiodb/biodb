# vi: fdm=marker

#' @include NcbiPubchemEntry.R

# Class declaration {{{1
################################################################

NcbiPubchemSubstEntry <- methods::setRefClass("NcbiPubchemSubstEntry", contains = "NcbiPubchemEntry")

# Constructor {{{1
################################################################

NcbiPubchemSubstEntry$methods( initialize = function(...) {

	callSuper(...)
	.self$addParsingExpression(BIODB.ACCESSION, "//PC-ID_id")
	#.self$addParsingExpression(BIODB.PUBCHEM.COMP.ID, "//PC-CompoundType_id_cid") --> Apparently there can be more than one CID for a substance.
})
