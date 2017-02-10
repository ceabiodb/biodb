# vi: fdm=marker

# Class declaration {{{1
################################################################

#'The mother abstract class of all NCBI connection classes.
#'@export
NcbiConn <- methods::setRefClass("NcbiConn", contains = "RemotedbConn", fields = list(.db.name = "character"))

# Constructor {{{1
################################################################

NcbiConn$methods( initialize = function(db.name = NA_character_, ...) {

	# From NCBI E-Utility manual: "In order not to overload the E-utility servers, NCBI recommends that users post no more than three URL requests per second and limit large jobs to either weekends or between 9:00 PM and 5:00 AM Eastern time during weekdays".
	scheduler <- UrlRequestScheduler$new(n = 3, parent = .self)

	# Call parent constructor
	callSuper(base.url = 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/', scheduler = scheduler, ...)

	# Set name
	if (is.null(db.name) || is.na(db.name))
		.self$message(MSG.ERROR, "You must set a name for this NCBI database.")
	.db.name <<- db.name
})

# Get entry ids {{{1
################################################################

NcbiConn$methods( getEntryIds = function(max.results = NA_integer_) {

	.self$message(MSG.CAUTION, "Method using a last resort solution for its implementation. Returns only a small subset of Ncbi entries.")

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

NcbiConn$methods( getNbEntries = function(count = FALSE) {

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
