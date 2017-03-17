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
		.self$message(MSG.ERROR, "Peakforest requires a token for this service.")

	url <- paste(.self$getBaseUrl(), 'compounds/', id,'?token=', .self$getToken(), sep = '')

	return(url)
})

# Get entry page url {{{1
################################################################

PeakforestCompoundConn$methods( getEntryPageUrl = function(id) {
	return(paste('https://peakforest.org/home?PFc=', id))
})
