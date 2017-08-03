# vi: fdm=marker

# CLASS DECLARATION {{{1
################################################################

WarningReporter <- methods::setRefClass("WarningReporter", contains = 'BiodbObserver')

# MESSAGE {{{1
################################################################

WarningReporter$methods( message = function(type = 'info', msg, class = NA_character_, method = NA_character_, level = 1) {

	.self$checkMessqgeType(type)

	# Raise warning
	if (type == 'warning') {
		caller.info <- if (is.na(class)) '' else class
		caller.info <- if (is.na(method)) caller.info else paste(caller.info, method, sep = '::')
		if (nchar(caller.info) > 0) caller.info <- paste('[', caller.info, '] ', sep = '')
		warning(paste0(caller.info, msg))
	}
})
