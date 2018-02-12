# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include NcbiPubchemConn.R
NcbiPubchemCompConn <- methods::setRefClass("NcbiPubchemCompConn", contains = "NcbiPubchemConn")

# Constructor {{{1
################################################################

NcbiPubchemCompConn$methods( initialize = function(...) {
	callSuper(db.name = 'compound', id.xmltag = 'PC-CompoundType_id_cid', entry.xmltag = 'PC-Compound', id.urlfield = 'cid', db.entrez.name = 'pccompound', ...)
})
