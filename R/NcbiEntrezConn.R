# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include NcbiConn.R
NcbiEntrezConn <- methods::setRefClass("NcbiEntrezConn", contains = "NcbiConn", fields = list(.db.entrez.name = "character"))

# Constructor {{{1
################################################################

NcbiEntrezConn$methods( initialize = function(db.entrez.name = NA_character_, ...) {

	# Call parent constructor
	callSuper(...)
	.self$.abstract.class('NcbiEntrezConn')

	# Set name
	if (is.null(db.entrez.name) || is.na(db.entrez.name))
		.self$message('error', "You must set a name for this NCBI database.")
	.db.entrez.name <<- db.entrez.name
})

# Web service efetch {{{1
################################################################

NcbiEntrezConn$methods( ws.efetch = function(id, rettype = NA_character_, retmode = NA_character_, biodb.parse = FALSE) {
	":\n\nCalls Entrez efetch web service. See https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EFetch."

	# Build request
	url <- paste('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/', 'efetch.fcgi', sep = '')
	params <- c(db = .self$.db.entrez.name, id = id)
	if ( ! is.na(rettype))
		params <- c(params, rettype = rettype)
	if ( ! is.na(retmode))
		params <- c(params, retmode = retmode)

	# Send request
	results <- .self$.getUrlScheduler()$getUrl(url, params)

	# Parse XML
	if (biodb.parse && retmode == 'xml')
		results <-  XML::xmlInternalTreeParse(results, asText = TRUE)

	return(results)
})

# Web service esearch {{{1
################################################################

NcbiEntrezConn$methods( ws.esearch = function(term, retmax = NA_integer_, biodb.parse = FALSE, biodb.ids = FALSE) {
	":\n\nCalls Entrez esearch web service. See https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch."

	# Build request
	url <- paste('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/', 'esearch.fcgi', sep = '')
	params <- c(db = .self$.db.entrez.name, term = term)
	if ( ! is.na(retmax))
		params <- c(params, retmax = retmax)

	# Send request
	results <- .self$.getUrlScheduler()$getUrl(url, params = params)

	# Parse XML
	if (biodb.parse || biodb.ids)
		results <-  XML::xmlInternalTreeParse(results, asText = TRUE)

	# Get IDs
	if (biodb.ids)
		results <- XML::xpathSApply(results, "//IdList/Id", XML::xmlValue)

	return(results)
})

# Web service einfo {{{1
################################################################

NcbiEntrezConn$methods( ws.einfo = function(biodb.parse = FALSE) {
	":\n\nCalls Entrez einfo web service. See https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EInfo."

	# Build request
	url <- paste('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/', 'einfo.fcgi', sep = '')
	params <- c(db = .self$.db.entrez.name, version = '2.0')

	# Send request
	results <- .self$.getUrlScheduler()$getUrl(url, params)

	# Parse XML
	if (biodb.parse)
		results <-  XML::xmlInternalTreeParse(results, asText = TRUE)

	return(results)
})

# Get entry ids {{{1
################################################################

NcbiEntrezConn$methods( getEntryIds = function(max.results = NA_integer_) {

	.self$message('caution', "Method using a last resort solution for its implementation. Returns only a small subset of Ncbi entries.")

	return(.self$ws.esearch(term = 'e', retmax = if (is.na(max.results)) 1000000 else max.results, biodb.ids = TRUE))
})

# Get nb entries {{{1
################################################################

NcbiEntrezConn$methods( getNbEntries = function(count = FALSE) {

	# Send request
	xml <- .self$ws.einfo(biodb.parse = TRUE)

	# Get number of elements
	n <- XML::xpathSApply(xml, "//Count", XML::xmlValue)
	n <- as.integer(n)

	return(n)
})

# Do get entry content url {{{1
################################################################

NcbiEntrezConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {
	return(.self$ws.efetch(id, retmode = 'xml'))
})

# Get entry content {{{1
################################################################

NcbiEntrezConn$methods( getEntryContent = function(entry.id) {

	# Initialize return values
	content <- rep(NA_character_, length(entry.id))

	# Get URLs
	urls <- .self$getEntryContentUrl(entry.id)

	# Request
	content <- vapply(urls, function(url) .self$.getUrlScheduler()$getUrl(url), FUN.VALUE = '')

	return(content)
})
