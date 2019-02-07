# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include NcbiConn.R
NcbiEntrezConn <- methods::setRefClass("NcbiEntrezConn", contains = "NcbiConn", fields = list(.entrez.name = "character", .entrez.tag = 'character', .entrez.id.tag = 'character'))

# Constructor {{{1
################################################################

NcbiEntrezConn$methods( initialize = function(entrez.name = NA_character_, entrez.tag = NA_character_, entrez.id.tag = NA_character_, ...) {

	# Call parent constructor
	callSuper(...)
	.self$.abstract.class('NcbiEntrezConn')

	# Set name
	if (is.null(entrez.name) || is.na(entrez.name))
		.self$message('error', "You must set an Entrez name for this NCBI database.")
	.entrez.name <<- entrez.name

	# Set tag
	.entrez.tag <<- if (is.null(entrez.tag)) NA_character_ else entrez.tag

	# Set ID tag
	.entrez.id.tag <<- if (is.null(entrez.id.tag)) NA_character_ else entrez.id.tag
})

# Web service efetch {{{1
################################################################

NcbiEntrezConn$methods( ws.efetch = function(id, rettype = NA_character_, retmode = NA_character_, biodb.parse = FALSE, biodb.url = FALSE) {
	":\n\nCalls Entrez efetch web service. See https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EFetch."

	# Build request
	url <- paste('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/', 'efetch.fcgi', sep = '')
	params <- c(db = .self$.entrez.name, id = paste(id, collapse = ','))
	if ( ! is.na(rettype))
		params <- c(params, rettype = rettype)
	if ( ! is.na(retmode))
		params <- c(params, retmode = retmode)

	# Returns URL
	if (biodb.url)
		return(.self$getBiodb()$getRequestScheduler()$getUrlString(url, params))

	# Send request
	results <- .self$getBiodb()$getRequestScheduler()$getUrl(url, params)

	# Parse XML
	if (biodb.parse && retmode == 'xml')
		results <-  XML::xmlInternalTreeParse(results, asText = TRUE)

	return(results)
})

# Web service esearch {{{1
################################################################

NcbiEntrezConn$methods( ws.esearch = function(term, field = NA_character_, retmax = NA_integer_, biodb.parse = FALSE, biodb.ids = FALSE) {
	":\n\nCalls Entrez esearch web service. See https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch."

	
	# Build request
	url <- paste('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/', 'esearch.fcgi', sep = '')
	params <- c(db = .self$.entrez.name, term = term)
	if ( ! is.na(field))
		params <- c(params, field = field)
	if ( ! is.na(retmax))
		params <- c(params, retmax = retmax)

	# Send request
	results <- .self$getBiodb()$getRequestScheduler()$getUrl(url, params = params)

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
	params <- c(db = .self$.entrez.name, version = '2.0')

	# Send request
	results <- .self$getBiodb()$getRequestScheduler()$getUrl(url, params)

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

# Do get entry content request {{{1
################################################################

NcbiEntrezConn$methods( .doGetEntryContentRequest = function(id, concatenate = TRUE) {

	if (concatenate)
		urls <- .self$ws.efetch(id, retmode = 'xml', biodb.url = TRUE)
	else
		urls <- vapply(id, function(single.id) .self$ws.efetch(single.id, retmode = 'xml', biodb.url = TRUE), FUN.VALUE = '')

	return(urls)
})

# Get entry content {{{1
################################################################

NcbiEntrezConn$methods( getEntryContent = function(entry.id) {

	# Debug
	.self$message('info', paste0("Get entry content(s) for ", length(entry.id)," id(s)..."))

	URL.MAX.LENGTH <- 2048
	concatenate <- TRUE
	done <- FALSE

	while ( ! done) {

		done <- TRUE

		# Initialize return values
		content <- rep(NA_character_, length(entry.id))

		# Get URL requests
		url.requests <- .self$getEntryContentRequest(entry.id, concatenate = concatenate, max.length = URL.MAX.LENGTH)

		# Loop on all URLs
		for (url in url.requests) {

			# Send request
			xmlstr <- .self$getBiodb()$getRequestScheduler()$getUrl(url)

			if (is.na(xmlstr) || length(grep('<ERROR>', xmlstr)) > 0) {
				if (concatenate) {
					.self$message('caution', "Something went wrong while downloading several entries at once.")
					concatenate <- FALSE
					done <- FALSE
					break
				}
				next
			}

			# Parse XML
			xml <-  XML::xmlInternalTreeParse(xmlstr, asText = TRUE)

			# Get returned IDs
			returned.ids <- XML::xpathSApply(xml, paste0("//", .self$.entrez.id.tag), XML::xmlValue)

			# Store contents
			content[match(returned.ids, entry.id)] <- vapply(XML::getNodeSet(xml, paste0("//", .self$.entrez.tag)), XML::saveXML, FUN.VALUE = '')
		}
	}

	return(content)
})
