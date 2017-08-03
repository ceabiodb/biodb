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

# Message {{{1
################################################################

BiodbObserver$methods( message = function(type = 'info', msg, class = NA_character_, method = NA_character_, level = 1) {
	.self$checkMessqgeType(type)
})

# Check message type {{{1
################################################################

BiodbObserver$methods( checkMessqgeType = function(type) {

	# Define allowed types
	allowed.types <- c('info', 'debug', 'caution', 'warning', 'error')

	# Is type unknown?
	if ( ! tolower(type) %in% allowed.types)
		error(paste("Unknown message type \"", type, "\". Please use one of: ", paste(allowed.types, collapse = ', '), '.', sep = ''))
})
