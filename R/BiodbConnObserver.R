# vi: fdm=marker ts=4 et cc=80

# Class declaration {{{1
################################################################################

#' The observer class of connectors.
#'
#' Classes inheriting this class, are able to register themselves to a connector instance and receiving messages from it.
#'
#' @seealso \code{\link{BiodbConn}}.
#'
#' @import methods
#' @export BiodbConnObserver
#' @exportClass BiodbConnObserver
BiodbConnObserver <- methods::setRefClass("BiodbConnObserver", fields = list())

# Terminating {{{1
################################################################################

BiodbConnObserver$methods( connTerminating = function(conn) {
})

# URLs updated {{{1
################################################################################

BiodbConnObserver$methods( connUrlsUpdated = function(conn) {
})

# Scheduler frequency updated {{{1
################################################################################

BiodbConnObserver$methods( connSchedulerFrequencyUpdated = function(conn) {
})
