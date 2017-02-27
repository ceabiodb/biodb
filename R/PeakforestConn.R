# vi: fdm=marker

# Class declaration {{{1
################################################################

#'PeakForest connector class.
#'@export
PeakforestConn <- methods::setRefClass("PeakforestConn", contains = c("RemotedbConn"))

# Constructor {{{1
################################################################

PeakforestConn$methods( initialize = function(...) {

	callSuper(content.type = BIODB.JSON, base.url = .self$getBiodb()$getConfig()$get(CFG.PEAKFOREST.URL), token = .self$getBiodb()$getConfig()$get(CFG.PEAKFOREST.TOKEN), ...)

	# Check token
	if (is.na(.self$getToken()))
		.self$message(MSG.CAUTION, "Peakforest requires a token to function correctly.")
})
