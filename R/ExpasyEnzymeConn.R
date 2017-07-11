# vi: fdm=marker

# Class declaration {{{1
################################################################

ExpasyEnzymeConn <- methods::setRefClass("ExpasyEnzymeConn", contains = "RemotedbConn")

# Constructor {{{1
################################################################

ExpasyEnzymeConn$methods( initialize = function(...) {

	callSuper(content.type = BIODB.TXT, base.url = "http://enzyme.expasy.org/", ...)
})

# Get entry content {{{1
################################################################

ExpasyEnzymeConn$methods( getEntryContent = function(entry.id) {

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

ExpasyEnzymeConn$methods( getEntryIds = function(max.results = NA_integer_) {

	# Send request
	html.results <- .self$.getUrlScheduler()$getUrl(paste(.self$getBaseUrl(), "enzyme-bycomment.html", sep = ''), params = c('e'))

	# Parse HTML
	xml <-  XML::htmlTreeParse(html.results, asText = TRUE, useInternalNodes = TRUE)

	# Get ids
	ids <- XML::xpathSApply(xml, "//a[starts-with(@href,'/EC/')]", XML::xmlValue)

	# Cut results
	if ( ! is.na(max.results) && length(ids) > max.results)
		ids <- ids[1:max.results]

	return(ids)
})

# Do get entry content url {{{1
################################################################

ExpasyEnzymeConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {

	url <- paste0(.self$getBaseUrl(), 'EC/', id, '.txt')

	return(url)
})
