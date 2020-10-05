#' The observer class of connectors.
#'
#' Classes inheriting this class are able to register themselves to a
#' connector instance and receive messages from it.
#' It is used by the scheduler class to receive updates about URLs and query
#' frequencies.
#'
#' @seealso \code{\link{BiodbConn}}.
#'
#' @import methods
BiodbConnObserver <- methods::setRefClass("BiodbConnObserver",

methods=list(

connTerminating=function(conn) {
},

connUrlsUpdated=function(conn) {
},

connSchedulerFrequencyUpdated=function(conn) {
}

))
