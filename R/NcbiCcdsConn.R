# vi: fdm=marker

#' @include NcbiConn.R

# Constants {{{1
################################################################

.BIODB.NCBI.CCDS.PARSING.EXPR <- list(
	'accession' = list(path = "//input[@id='DATA']", attr = "value"),
	'sequence'  = "//b[starts-with(.,'Nucleotide Sequence')]/../tt"
)

# Class declaration {{{1
################################################################

NcbiCcdsConn <- methods::setRefClass("NcbiCcdsConn", contains = "NcbiConn")

# Constructor {{{1
################################################################

NcbiCcdsConn$methods( initialize = function(...) {

	# Call parent constructor
	callSuper(...)
})


# Do get entry content request {{{1
################################################################

NcbiCcdsConn$methods( .doGetEntryContentRequest = function(id, concatenate = TRUE) {
	return(.self$getEntryPageUrl(id))
})

# Get entry page url {{{1
################################################################

NcbiCcdsConn$methods( getEntryPageUrl = function(id) {
	return(vapply(id, function(x) BiodbUrl(url = c(.self$getUrl('base.url'), 'CcdsBrowse.cgi'), params = list(REQUEST = 'CCDS', GO = 'MainBrowse', DATA = x))$toString(), FUN.VALUE = ''))
})

# Get entry image url {{{1
################################################################

NcbiCcdsConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})

# Private methods {{{1
################################################################

# Get parsing expressions {{{2
################################################################

NcbiCcdsConn$methods( .getParsingExpressions = function() {
	return(.BIODB.NCBI.CCDS.PARSING.EXPR)
})

# Get entry ids {{{2
################################################################

NcbiCcdsConn$methods( .doGetEntryIds = function(max.results = NA_integer_) {
	return(NULL)
})

