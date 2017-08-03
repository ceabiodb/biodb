# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include NcbiConn.R
NcbiEntrezConn <- methods::setRefClass("NcbiEntrezConn", contains = "NcbiConn", fields = list(.db.name = "character"))

# Constructor {{{1
################################################################

NcbiEntrezConn$methods( initialize = function(db.name = NA_character_, ...) {

	# Call parent constructor
	callSuper(base.url = 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/', ...)

	# Set name
	if (is.null(db.name) || is.na(db.name))
		.self$message('error', "You must set a name for this NCBI database.")
	.db.name <<- db.name
})

# Get entry ids {{{1
################################################################

NcbiEntrezConn$methods( getEntryIds = function(max.results = NA_integer_) {

	.self$message('caution', "Method using a last resort solution for its implementation. Returns only a small subset of Ncbi entries.")

	# Send request
	xmlstr <- .self$.getUrlScheduler()$getUrl(paste(.self$getBaseUrl(), 'esearch.fcgi?db=', .self$.db.name, '&term=e&retmax=', if (is.na(max.results)) 1000000 else max.results, sep = ''))

	# Parse XML
	xml <-  XML::xmlInternalTreeParse(xmlstr, asText = TRUE)

	# Get IDs
	ids <- XML::xpathSApply(xml, "//IdList/Id", XML::xmlValue)

	return(ids)
})

# Get nb entries {{{1
################################################################

NcbiEntrezConn$methods( getNbEntries = function(count = FALSE) {

	n <- NA_integer_

	# Send request
	xmlstr <- .self$.getUrlScheduler()$getUrl(paste(.self$getBaseUrl(), 'einfo.fcgi?db=', .self$.db.name, '&version=2.0', sep = ''))

	# Parse XML
	xml <-  XML::xmlInternalTreeParse(xmlstr, asText = TRUE)

	# Get number of elements
	n <- XML::xpathSApply(xml, "//Count", XML::xmlValue)
	n <- as.integer(n)

	return(n)
})
