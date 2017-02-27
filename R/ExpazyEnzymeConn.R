# vi: fdm=marker

# Class declaration {{{1
################################################################

ExpazyEnzymeConn <- methods::setRefClass("ExpazyEnzymeConn", contains = "RemotedbConn")

# Constructor {{{1
################################################################

ExpazyEnzymeConn$methods( initialize = function(...) {

	callSuper(content.type = BIODB.TXT, base.url = "http://enzyme.expasy.org/", ...)
})

# Get entry content {{{1
################################################################

ExpazyEnzymeConn$methods( getEntryContent = function(id) {

	# Initialize return values
	content <- rep(NA_character_, length(id))

	# Request
	content <- vapply(id, function(x) .self$.get.url(get.entry.url(BIODB.ENZYME, accession = x, content.type = BIODB.TXT)), FUN.VALUE = '')

	return(content)
})

# Create entry {{{1
################################################################

ExpazyEnzymeConn$methods( createEntry = function(content, drop = TRUE) {
	return(createEnzymeEntryFromTxt(.self$getBiodb(), content, drop = drop))
})

# Get entry ids {{{1
################################################################

ExpazyEnzymeConn$methods( getEntryIds = function(max.results = NA_integer_) {

	# Send request
	html.results <- .self$.getUrlScheduler()$getUrl(paste(.self$getBaseUrl(), "enzyme-bycomment.html", sep = ''), params = c('e'))

	# Parse HTML
	xml <-  XML::htmlTreeParse(html.results, asText = TRUE, useInternalNodes = TRUE)

	# Get ids
	ids <- XML::xpathSApply(xml, "//a[starts-with(@href,'/EC/')]", XML::xmlValue)
	.self$message(MSG.DEBUG, paste('ENZYME IDS =', paste(ids, collapse = "\n")))

	# Cut results
	if ( ! is.na(max.results) && length(ids) > max.results)
		ids <- ids[1:max.results]

	return(ids)
})
