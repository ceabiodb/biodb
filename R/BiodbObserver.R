# vi: fdm=marker

# Class declaration {{{1
################################################################

#' The mother abstract class of all observer classes.
#'
#' This abstract class defines all the methods that can be used to send messages to the observers. You can define new observer classes by inherting from this class.
#'
#' @param type      The type of a message. It must be one of: 'info', 'debug', 'caution', 'warning', 'error'.
#' @param msg       The text message to send.
#' @param class     The class of the object that called this message method.
#' @param method    The method of that called this message method.
#'
#' @seealso \code{\link{BiodbLogger}}, \code{\link{BiodbWarningReporter}}, \code{\link{BiodbErrorReporter}}.
#'
#' @examples
#' # Define a new observer class
#' MyObsClass <- methods::setRefClass("MyObsClass", contains = 'BiodbObserver')
#'
#' # Define the message method
#' MyObsClass$methods( message = function(type = 'info', msg,
#'                                        class = NA_character_, method = NA_character_) {
#' .self$checkMessageType(type)
#' # print(paste(type, msg, sep = ': '))
#' })
#'
#' # Create an instance and register an instance of the new observer class:
#' mybiodb <- biodb::Biodb(observers = MyObsClass$new())
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @export BiodbObserver
#' @exportClass BiodbObserver
BiodbObserver <- methods::setRefClass("BiodbObserver", fields = list())

# Terminate {{{1
################################################################

BiodbObserver$methods( terminate = function() {
})

# Message {{{1
################################################################

BiodbObserver$methods( message = function(type = 'info', msg, class = NA_character_, method = NA_character_) {
	.self$checkMessageType(type)
})

# Info progress {{{1
################################################################

BiodbObserver$methods( progress = function(type = 'info', msg, index, total, first) {
	.self$checkMessageType(type)
})

# Check message type {{{1
################################################################

BiodbObserver$methods( checkMessageType = function(type) {

	# Define allowed types
	allowed.types <- c('info', 'debug', 'caution', 'warning', 'error')

	# Is type unknown?
	if ( ! tolower(type) %in% allowed.types)
		stop(paste("Unknown message type \"", type, "\". Please use one of: ", paste(allowed.types, collapse = ', '), '.', sep = ''))
})
