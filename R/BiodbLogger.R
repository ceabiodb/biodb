# vi: fdm=marker

##########################
# CLASS DECLARATION {{{1 #
##########################

BiodbLogger <- methods::setRefClass("BiodbLogger", contains = 'BiodbObserver', fields = list(.verbose.level = 'integer', .debug.level = 'integer', .file = 'ANY', .fail.on.error = 'logical', .signal.warnings = 'logical'))

####################
# CONSTRUCTOR {{{1 #
####################

BiodbLogger$methods( initialize = function(verbose.level = 1, debug.level = 1, file = NULL, ...) {

	.verbose.level <<- if ( ! is.null(verbose.level) && ! is.na(verbose.level)) verbose.level else 1
	.debug.level <<- if ( ! is.null(debug.level) && ! is.na(debug.level)) debug.level else 1
	.file <<- if ( ! is.null(file) && ! is.na(file)) file else stderr()
	.fail.on.error <<- TRUE
	.signal.warnings <<- TRUE

	callSuper(...)
})

################
# MESSAGE {{{1 #
################

BiodbLogger$methods( message = function(type = MSG.INFO, msg, level = 1) {
	type %in% biodb::MSG.TYPES || .self$message(biodb::MSG.ERROR, paste0("Unknown message type ", type, "."))

	display = TRUE
	if (type == biodb::MSG.INFO && .self$.verbose.level < level)
		display = FALSE
	if (type == biodb::MSG.DEBUG && .self$.debug.level < level)
		display = FALSE

	if (display)
		cat(type, ': ', msg, "\n", sep = '', file = .self$.file)

	# Raise error
	if (type == biodb::MSG.ERROR && .self$.fail.on.error)
		stop(msg)

	# Raise warning
	if (type == biodb::MSG.WARNING && .self$.signal.warnings)
		warning(msg)
})
