# vi: fdm=marker

# Class declaration {{{1
################################################################

#' The mother abstract class of all observer classes.
#'
#' @seealso \code{\link{BiodbLogger}}.
#'
#' @import methods
#' @export BiodbObserver
#' @exportClass BiodbObserver
BiodbObserver <- methods::setRefClass("BiodbObserver", fields = list())

# Constants {{{1
################################################################

MSG.INFO <- 'INFO'
MSG.DEBUG <- 'DEBUG'
MSG.CAUTION <- 'CAUTION'
MSG.WARNING <- 'WARNING'
MSG.ERROR <- 'ERROR'

.MSG.TYPES <- c(MSG.ERROR, MSG.WARNING, MSG.CAUTION, MSG.DEBUG, MSG.INFO)

# Message {{{1
################################################################

BiodbObserver$methods( message = function(type = MSG.INFO, msg, class = NA_character_, method = NA_character_, level = 1) {
})
