# vi: fdm=marker

# Class declaration {{{1
################################################################

#' A class for logging biodb messages either to standard stream or into a file.
#'
#' This class implements a logger for the biodb package. When creating an instance of this class, you can choose to either log to a standard stream (standard error or standard output) or to a file. See section Fields for a list of the constructor's parameters.
#'
#' @field file The file path, file connection or standard stream to which you want to send biodb messages.
#' @field mode For a file, the mode in which you want to open the file. 'w' for writing (if a file already exists, it will be erased), and 'a' for appending (if a file already exists, logs will be appended to it).
#'
#' @seealso \code{\link{Biodb}}, \code{\link{BiodbObserver}}.
#'
#' @examples
#' # Create a biodb instance with a file log
#' mybiodb <- biodb::Biodb(observers = biodb::BiodbLogger(file = "myfile.log"))
#'
#' # Terminate instance.
#' mybiodb$terminate()
#' 
#' # Create a biodb instance with logging to standard output
#' mybiodb <- biodb::Biodb(observers = biodb::BiodbLogger(file = stdout()))
#'
#' # Terminate instance.
#' mybiodb$terminate()
#' 
#' @import methods
#' @include BiodbObserver.R
#' @export BiodbLogger
#' @exportClass BiodbLogger
BiodbLogger <- methods::setRefClass("BiodbLogger", contains = 'BiodbObserver', fields = list(.file = 'ANY', .exclude = 'character', .close.file = 'logical', .lastime.progress = 'list', .progress.laptime = 'integer', .progress.initial.time = 'list'))

# Constructor {{{1
################################################################

BiodbLogger$methods( initialize = function(file = stderr(), mode = 'w', close.file = TRUE, ...) {

	callSuper(...)

	# Is file NULL or NA?
	if (is.null(file) || is.na(file))
		error("You must choose either a standard stream or a file path for the \"file\" parameter")

	# File is a path => create a connection
	if (is.character(file))
		file <- file(file, open = mode)

	# Check file class type
	if ( ! all(class(file) %in% c('file', 'connection', 'terminal')))
		error(paste('Unknown class "', class(file), '" for log file.', sep = ''))

	# Set member field
	.file <<- file
	.close.file <<- close.file
	.lastime.progress <<- list()
	.progress.initial.time <<- list()
	.progress.laptime <<- as.integer(10) # In seconds

	# Exclude DEBUG messages
	.exclude <<- character(0)
	.self$excludeMsgType('debug')
})

# Terminate {{{1
################################################################

BiodbLogger$methods( terminate = function() {
	if ( .self$.close.file && ! is.null(.self$.file) && class(.self$.file)[[1]] != 'terminal')
		close(.self$.file)
})

# Exclude message type {{{1
################################################################

BiodbLogger$methods( excludeMsgType = function(type) {

	.self$checkMessageType(type)

	if ( ! type %in% .self$.exclude)
		.self$.exclude <- c(.self$.exclude, type)
})

# Include message type {{{1
################################################################

BiodbLogger$methods( includeMsgType = function(type) {

	.self$checkMessageType(type)

	if (type %in% .self$.exclude)
		.self$.exclude <- .self$.exclude[ ! .self$.exclude %in% type]
})

# Message {{{1
################################################################

BiodbLogger$methods( message = function(type = 'info', msg, class = NA_character_, method = NA_character_) {

	.self$checkMessageType(type)

	if ( ! type %in% .self$.exclude) {

		# Set caller information
		caller.info <- if (is.na(class)) '' else class
		caller.info <- if (is.na(method)) caller.info else paste(caller.info, method, sep = '::')
		if (nchar(caller.info) > 0) caller.info <- paste('[', caller.info, '] ', sep = '')

		# Set timestamp
		timestamp <- paste('[', as.POSIXlt(Sys.time()), ']', sep = '')

		# Output message
		cat(toupper(type), timestamp, caller.info, ': ', msg, "\n", sep = '', file = .self$.file)
	}
})

# Info progress {{{1
################################################################

BiodbLogger$methods( progress = function(type = 'info', msg, index, total, first) {

	if (first || ! msg %in% names(.self$.lastime.progress))
		.self$.lastime.progress[[msg]] = .self$.progress.initial.time[[msg]] = Sys.time()

	else if (Sys.time() - .self$.lastime.progress[[msg]] > .self$.progress.laptime) {
		eta = Sys.time() + (total - index) * (Sys.time() - .self$.progress.initial.time[[msg]]) / index
		.self$message(type, paste0(msg, ' ', index, ' / ', total, ' (', ((100 * index) %/% total), '%, ETA: ', eta, ').'))
		.self$.lastime.progress[[msg]] = Sys.time()
	}
})
