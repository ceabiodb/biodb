# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbConnObserver {{{1
################################################################################

# Declaration {{{2
################################################################################

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

# Public methods {{{2
################################################################################

methods=list(

# Terminating {{{3
################################################################################

connTerminating=function(conn) {
},

# URLs updated {{{3
################################################################################

connUrlsUpdated=function(conn) {
},

# Scheduler frequency updated {{{3
################################################################################

connSchedulerFrequencyUpdated=function(conn) {
}

))
