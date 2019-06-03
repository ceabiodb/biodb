# vi: fdm=marker ts=4 et cc=80

# BiodbConnObserver {{{1
################################################################################

#' The observer class of connectors.
#'
#' Classes inheriting this class, are able to register themselves to a
#' connector instance and receiving messages from it.
#'
#' @seealso \code{\link{BiodbConn}}.
#'
#' @import methods
#' @export BiodbConnObserver
#' @exportClass BiodbConnObserver
BiodbConnObserver <- methods::setRefClass("BiodbConnObserver",

# Public methods {{{2
################################################################################

methods = list(

# Terminating {{{3
################################################################################

connTerminating = function(conn) {
},

# URLs updated {{{3
################################################################################

connUrlsUpdated = function(conn) {
},

# Scheduler frequency updated {{{3
################################################################################

connSchedulerFrequencyUpdated = function(conn) {
}

))
