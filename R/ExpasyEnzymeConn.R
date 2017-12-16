# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include CompounddbConn.R
#' @include RemotedbConn.R
ExpasyEnzymeConn <- methods::setRefClass("ExpasyEnzymeConn", contains = c("RemotedbConn", "CompounddbConn"))

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

# Web service enzyme-search-de {{{1
################################################################

ExpasyEnzymeConn$methods( ws.enzymeSearchDE = function(de, biodb.ids = FALSE) {

	# Send request
	html.results <- .self$.getUrlScheduler()$getUrl(paste(.self$getBaseUrl(), "cgi-bin/enzyme/enzyme-search-de", sep = ''), params = de)

	# Parse biodb IDs
	if (biodb.ids) {

		ids <- .self$.parseWsReturnedHtml(html.results)

		return(ids)
	}

	return(html.results)
})

# Get entry ids {{{1
################################################################

ExpasyEnzymeConn$methods( getEntryIds = function(max.results = NA_integer_) {

	# Send request
	html.results <- .self$.getUrlScheduler()$getUrl(paste(.self$getBaseUrl(), "enzyme-bycomment.html", sep = ''), params = 'e')

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

# Parse HTML returned by web services {{{1
################################################################

ExpasyEnzymeConn$methods( .parseWsReturnedHtml = function(html.results) {

	# Parse HTML
	xml <-  XML::htmlTreeParse(html.results, asText = TRUE, useInternalNodes = TRUE)

	# Get ids
	ids <- XML::xpathSApply(xml, "//a[starts-with(@href,'/EC/')]", XML::xmlValue)

	return(ids)
})
