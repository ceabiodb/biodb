# vi: fdm=marker

# Class declaration {{{1
################################################################

PubchemSubstConn <- methods::setRefClass("PubchemSubstConn", contains = "PubchemConn")

# Constructor {{{1
################################################################

PubchemSubstConn$methods( initialize = function(...) {
	callSuper(content.type = BIODB.XML, db.name = 'substance', id.xmltag = 'PC-ID_id', entry.xmltag = 'PC-Substance', id.urlfield = 'sid', ...)
})

# Create entry {{{1
################################################################

PubchemSubstConn$methods( createEntry = function(content, drop = TRUE) {
	return(createPubchemSubstanceFromXml(.self$getBiodb(), content, drop = drop))
})
