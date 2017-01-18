# vi: fdm=marker

# CLASS DECLARATION {{{1
################################################################

WarningReporter <- methods::setRefClass("WarningReporter", contains = 'BiodbObserver')

# MESSAGE {{{1
################################################################

WarningReporter$methods( message = function(type = MSG.INFO, msg, class = NA_character_, level = 1) {

	# Raise warning
	if (type == MSG.WARNING) {
		class.info <- if (is.na(class)) '' else paste0('[', class, '] ')
		warning(paste0(classinfo, msg))
	}
})
