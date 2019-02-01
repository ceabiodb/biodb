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


# Get entry ids {{{1
################################################################

NcbiCcdsConn$methods( getEntryIds = function(max.results = NA_integer_) {
	return(NULL)
})

# Do get entry content request {{{1
################################################################

NcbiCcdsConn$methods( .doGetEntryContentRequest = function(id, concatenate = TRUE) {
	return(.self$getEntryPageUrl(id))
})

# Get entry page url {{{1
################################################################

NcbiCcdsConn$methods( getEntryPageUrl = function(id) {
	return(paste0(file.path(.self$getBaseUrl(), 'CcdsBrowse.cgi', fsep = '/'), '?REQUEST=CCDS&GO=MainBrowse&DATA=', id))
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
