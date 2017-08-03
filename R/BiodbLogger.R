# vi: fdm=marker

# Class declaration {{{1
################################################################

#' A class for logging biodb messages either to standard error or into a file.
#'
#' @import methods
#' @include BiodbObserver.R
#' @export BiodbLogger
#' @exportClass BiodbLogger
BiodbLogger <- methods::setRefClass("BiodbLogger", contains = 'BiodbObserver', fields = list(.file = 'ANY'))

# Constructor {{{1
################################################################

BiodbLogger$methods( initialize = function(file = NULL, mode = 'w', ...) {

	callSuper(...)

	# Set file
	if (is.null(file) || is.na(file))
		file <- stderr()
	if (is.character(file))
		file <- file(file, open = mode)
	if ( ! all(class(file) %in% c('file', 'connection', 'terminal')))
		.self$message('error', paste('Unknown class "', class(file), '" for log file.', sep = ''))
	.file <<- file
})

# Message {{{1
################################################################

BiodbLogger$methods( message = function(type = 'info', msg, class = NA_character_, method = NA_character_) {

	.self$checkMessqgeType(type)

	# Set caller information
	caller.info <- if (is.na(class)) '' else class
	caller.info <- if (is.na(method)) caller.info else paste(caller.info, method, sep = '::')
	if (nchar(caller.info) > 0) caller.info <- paste('[', caller.info, '] ', sep = '')

	# Output message
	cat(type, caller.info, ': ', msg, "\n", sep = '', file = .self$.file)
})
