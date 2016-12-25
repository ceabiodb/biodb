# vi: fdm=marker

# CLASS DECLARATION {{{1
################################################################

BiodbLogger <- methods::setRefClass("BiodbLogger", contains = 'BiodbObserver', fields = list(.verbose.level = 'integer', .debug.level = 'integer', .file = 'ANY'))

# CONSTRUCTOR {{{1
################################################################

BiodbLogger$methods( initialize = function(verbose.level = as.integer(1), debug.level = as.integer(1), file = NULL, ...) {

	.verbose.level <<- if ( ! is.null(verbose.level) && ! is.na(verbose.level)) verbose.level else as.integer(1)
	.debug.level <<- if ( ! is.null(debug.level) && ! is.na(debug.level)) debug.level else as.integer(1)
	.file <<- if ( ! is.null(file) && ! is.na(file)) file else stderr()

	callSuper(...)
})

# MESSAGE {{{1
################################################################

BiodbLogger$methods( message = function(type = MSG.INFO, msg, class = NA_character_, level = 1) {

	# Check message type
	type %in% .MSG.TYPES || .self$message(MSG.ERROR, paste0("Unknown message type ", type, "."))

	# Should message be displayed ?
	display = TRUE
	if (type == MSG.INFO && .self$.verbose.level < level)
		display = FALSE
	if (type == MSG.DEBUG && .self$.debug.level < level)
		display = FALSE

	# Display message
	if (display) {
		class.info <- if (is.na(class)) '' else paste0('[', class, ']')
		cat(type, class.info, ': ', msg, "\n", sep = '', file = .self$.file)
	}
})
