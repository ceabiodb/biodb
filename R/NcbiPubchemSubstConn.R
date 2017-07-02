# vi: fdm=marker

# Class declaration {{{1
################################################################

NcbiPubchemSubstConn <- methods::setRefClass("NcbiPubchemSubstConn", contains = "NcbiPubchemConn")

# Constructor {{{1
################################################################

NcbiPubchemSubstConn$methods( initialize = function(...) {
	callSuper(content.type = BIODB.XML, db.name = 'substance', id.xmltag = 'PC-ID_id', entry.xmltag = 'PC-Substance', id.urlfield = 'sid', ...)
})

# Create entry {{{1
################################################################

NcbiPubchemSubstConn$methods( createEntry = function(content, drop = TRUE) {
	return(createNcbiPubchemSubstanceFromXml(.self$getBiodb(), content, drop = drop))
})

# Get entry ids {{{1
################################################################

NcbiPubchemSubstConn$methods( getEntryIds = function(max.results = NA_integer_) {
	.self$message(MSG.CAUTION, "No method implemented for computing list of IDs.")
	return(NULL)
})
