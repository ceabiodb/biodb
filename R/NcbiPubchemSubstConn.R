# vi: fdm=marker

# Class declaration {{{1
################################################################

NcbiPubchemSubstConn <- methods::setRefClass("NcbiPubchemSubstConn", contains = "NcbiPubchemConn")

# Constructor {{{1
################################################################

NcbiPubchemSubstConn$methods( initialize = function(...) {
	callSuper(db.name = 'substance', id.xmltag = 'PC-ID_id', entry.xmltag = 'PC-Substance', id.urlfield = 'sid', db.entrez.name = 'pcsubstance', ...)
})
