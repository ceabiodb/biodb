# vi: fdm=marker

# Class declaration {{{1
################################################################

PubchemCompConn <- methods::setRefClass("PubchemCompConn", contains = "PubchemConn")

# Constructor {{{1
################################################################

PubchemCompConn$methods( initialize = function(...) {
	callSuper(content.type = BIODB.XML, db.name = 'compound', id.xmltag = 'PC-CompoundType_id_cid', entry.xmltag = 'PC-Compound', id.urlfield = 'cid', ...)
})

# Create entry {{{1
################################################################

PubchemCompConn$methods( createEntry = function(content, drop = TRUE) {
	return(createPubchemEntryFromXml(.self$getBiodb(), content, drop = drop))
})

# Get entry ids {{{1
################################################################

PubchemCompConn$methods( getEntryIds = function(max.results = NA_integer_) {
	.self$message(MSG.CAUTION, "No method implemented for computing list of IDs.")
	return(NULL)
})
