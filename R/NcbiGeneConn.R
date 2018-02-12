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
