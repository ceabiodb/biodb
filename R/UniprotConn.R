# vi: fdm=marker

# Class declaration {{{1
################################################################

UniprotConn <- methods::setRefClass("UniprotConn", contains = "RemotedbConn")

# Constructor {{{1
################################################################

UniprotConn$methods( initialize = function(...) {
	callSuper(content.type = BIODB.XML, base.url = 'http://www.uniprot.org/uniprot/', ...)
})

# Get entry content {{{1
################################################################

UniprotConn$methods( getEntryContent = function(entry.id) {

	# Initialize return values
	content <- rep(NA_character_, length(entry.id))

	# Get URLs
	urls <- .self$getEntryContentUrl(entry.id)
	
	# Request
	content <- vapply(urls, function(url) .self$.getUrlScheduler()$getUrl(url), FUN.VALUE = '')

	return(content)
})

# Create entry {{{1
################################################################

UniprotConn$methods( createEntry = function(content, drop = TRUE) {
	return(createUniprotEntryFromXml(.self$getBiodb(), content, drop = drop))
})

# Get entry ids {{{1
################################################################

UniprotConn$methods( getEntryIds = function(max.results = NA_integer_) {
	.self$message(MSG.CAUTION, "No method implemented for computing list of IDs.")
	return(NULL)
})

# Do get entry content url {{{1
################################################################

UniprotConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {
	                    
	url <- paste0(.self$getBaseUrl(), id, '.xml')

	return(url)
})
