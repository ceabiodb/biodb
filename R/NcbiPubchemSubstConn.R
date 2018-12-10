# vi: fdm=marker

# Constants {{{1
################################################################

.BIODB.PUBCHEM.SUBST.PARSING.EXPR <- list(
	'accession'             = "//PC-ID_id",
	'ncbi.pubchem.comp.id'  = "//PC-CompoundType_id_cid"
)

# Class declaration {{{1
################################################################

NcbiPubchemSubstConn <- methods::setRefClass("NcbiPubchemSubstConn", contains = "NcbiPubchemConn")

# Constructor {{{1
################################################################

NcbiPubchemSubstConn$methods( initialize = function(...) {
	callSuper(db.name = 'substance', id.xmltag = 'PC-ID_id', entry.xmltag = 'PC-Substance', id.urlfield = 'sid', entrez.name = 'pcsubstance', ...)
})

# Private methods {{{1
################################################################

# Get parsing expressions {{{2
################################################################

NcbiPubchemSubstConn$methods( .getParsingExpressions = function() {
	return(.BIODB.PUBCHEM.SUBST.PARSING.EXPR)
})
