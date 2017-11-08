# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include CompounddbConn.R
#' @include RemotedbConn.R
UniprotConn <- methods::setRefClass("UniprotConn", contains = c("RemotedbConn", "CompounddbConn"))

# Constructor {{{1
################################################################

UniprotConn$methods( initialize = function(...) {
	callSuper(...)
})

# Query entries {{{1
################################################################

UniprotConn$methods( queryEntries = function() {
	"Direct query to the database for searching for queries. See http://www.uniprot.org/help/api_queries for details."

	"http://www.uniprot.org/uniprot/?query=&columns=id&format=tab&limit=10"
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


# Get entry ids {{{1
################################################################

UniprotConn$methods( getEntryIds = function(max.results = NA_integer_) {
	.self$message('caution', "No method implemented for computing list of IDs.")
	return(NULL)
})

# Do get entry content url {{{1
################################################################

UniprotConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {
	                    
	url <- paste0(.self$getBaseUrl(), id, '.xml')

	return(url)
})
