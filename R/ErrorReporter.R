# vi: fdm=marker

# CLASS DECLARATION {{{1
################################################################

ErrorReporter <- methods::setRefClass("ErrorReporter", contains = 'BiodbObserver')

# MESSAGE {{{1
################################################################

ErrorReporter$methods( message = function(type = MSG.INFO, msg, level = 1) {

	# Raise error
	if (type == biodb::MSG.ERROR)
		stop(msg)
})
