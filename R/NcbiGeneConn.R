# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include NcbiEntrezConn.R
#' @include CompounddbConn.R
NcbiGeneConn <- methods::setRefClass("NcbiGeneConn", contains = c('NcbiEntrezConn', 'CompounddbConn'))

# Constructor {{{1
################################################################

NcbiGeneConn$methods( initialize = function(...) {

	callSuper(db.entrez.name = 'gene', ...)
})

# Get entry page url {{{1
################################################################

NcbiGeneConn$methods( getEntryPageUrl = function(id) {
	return(paste0(.self$getBaseUrl(), .self$.db.entrez.name, '/?term=', id))
})

# Get entry image url {{{1
################################################################

NcbiGeneConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})

# Search compound {{{1
################################################################

NcbiGeneConn$methods( searchCompound = function(name = NULL, molecular.mass = NULL, monoisotopic.mass = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {
	":\n\nSearch for compounds by name and/or by mass."

	ids <- NULL

	# Search by name
	if ( ! is.null(name))
		term <- paste0('"', name, '"', '[Gene Name]')

	# Search by mass
	if ( ! is.null(monoisotopic.mass) || ! is.null(molecular.mass))
		.self$message('caution', 'Search by mass is not possible.')

	# Set retmax
	if (is.na(max.results)) {
		xml <- .self$ws.esearch(term = term, retmax = 0, biodb.parse = TRUE)
		retmax <- as.integer(XML::xpathSApply(xml, "/eSearchResult/Count", XML::xmlValue))
		if (length(retmax) == 0)
			retmax = NA_integer_
	}
	else
		retmax <- max.results

	# Send request
	ids <- .self$ws.esearch(term = term, retmax = retmax, biodb.ids = TRUE)

	return(ids)
})
