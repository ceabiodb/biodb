# vi: fdm=marker

# Class declaration {{{1
################################################################

NcbiGeneConn <- methods::setRefClass("NcbiGeneConn", contains = "NcbiEntrezConn")

# Constructor {{{1
################################################################

NcbiGeneConn$methods( initialize = function(...) {

	callSuper(content.type = BIODB.XML, db.name = 'gene', ...)
})

#####################
# GET ENTRY CONTENT #
#####################

NcbiGeneConn$methods( getEntryContent = function(id) {

	# Initialize return values
	content <- rep(NA_character_, length(id))

	# Request
	content <- vapply(id, function(x) .self$.get.url(get.entry.url(BIODB.NCBIGENE, x, content.type = BIODB.XML)), FUN.VALUE = '')

	return(content)
})

################
# CREATE ENTRY #
################

NcbiGeneConn$methods( createEntry = function(content, drop = TRUE) {
	return(createNcbigeneEntryFromXml(.self$getBiodb(), content, drop = drop))
})
