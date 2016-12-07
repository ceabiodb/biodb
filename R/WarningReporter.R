# vi: fdm=marker

# CLASS DECLARATION {{{1
################################################################

WarningReporter <- methods::setRefClass("WarningReporter", contains = 'BiodbObserver')

# MESSAGE {{{1
################################################################

WarningReporter$methods( message = function(type = MSG.INFO, msg, level = 1) {

	# Raise warning
	if (type == biodb::MSG.WARNING)
		warning(msg)
})
