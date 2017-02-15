#####################
# CLASS DECLARATION #
#####################

UniprotConn <- methods::setRefClass("UniprotConn", contains = "RemotedbConn")

# Constructor {{{1
################################################################

UniprotConn$methods( initialize = function(...) {
	callSuper(content.type = BIODB.XML, ...)
})

#####################
# GET ENTRY CONTENT #
#####################

UniprotConn$methods( getEntryContent = function(ids) {

	# Initialize return values
	content <- rep(NA_character_, length(ids))

	# Request
	content <- vapply(ids, function(x) .self$.get.url(get.entry.url(BIODB.UNIPROT, x, content.type = BIODB.XML)), FUN.VALUE = '')

	return(content)
})

################
# CREATE ENTRY #
################

UniprotConn$methods( createEntry = function(content, drop = TRUE) {
	return(createUniprotEntryFromXml(.self$getBiodb(), content, drop = drop))
})
