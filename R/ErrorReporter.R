# vi: fdm=marker

# CLASS DECLARATION {{{1
################################################################

ErrorReporter <- methods::setRefClass("ErrorReporter", contains = 'BiodbObserver')

# MESSAGE {{{1
################################################################

ErrorReporter$methods( message = function(type = MSG.INFO, msg, class = NA_character_, level = 1) {

	# Raise error
	if (type == biodb::MSG.ERROR) {
		class.info <- if (is.na(class)) '' else paste0('[', class, '] ')
		stop(paste0(classinfo, msg))
	}
})
