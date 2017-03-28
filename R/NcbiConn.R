# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include RemotedbConn.R
NcbiConn <- methods::setRefClass("NcbiConn", contains = "RemotedbConn")

# Constructor {{{1
################################################################

NcbiConn$methods( initialize = function(...) {

	# From NCBI E-Utility manual: "In order not to overload the E-utility servers, NCBI recommends that users post no more than three URL requests per second and limit large jobs to either weekends or between 9:00 PM and 5:00 AM Eastern time during weekdays".
	scheduler <- UrlRequestScheduler$new(n = 3, parent = .self)

	# Call parent constructor
	callSuper(scheduler = scheduler, ...)
})
