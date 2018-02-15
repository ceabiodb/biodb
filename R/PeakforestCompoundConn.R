# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include PeakforestConn.R
#' @include CompounddbConn.R
PeakforestCompoundConn <- methods::setRefClass("PeakforestCompoundConn", contains = c("PeakforestConn", "CompounddbConn"))

# Constructor {{{1
################################################################

PeakforestCompoundConn$methods( initialize = function(...) {

	callSuper(db.name = 'compounds', ...)
})

# Get entry content url {{{1
################################################################

PeakforestCompoundConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {

	# Check token
	if (is.na(.self$getToken()))
		.self$message('error', "Peakforest requires a token for this service.")

	url <- paste(.self$getBaseUrl(), 'compounds/', id,'?token=', .self$getToken(), sep = '')

	return(url)
})

# Get entry page url {{{1
################################################################

PeakforestCompoundConn$methods( getEntryPageUrl = function(id) {
	return(paste('https://peakforest.org/home?PFc=', id))
})

# Get entry image url {{{1
################################################################

PeakforestCompoundConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})

# Search compound {{{1
################################################################

CompounddbConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.field = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {

	ids <- NULL

	# Search by name
	if ( ! is.null(name))

	return(ids)
})
