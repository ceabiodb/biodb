# vi: fdm=marker

# CLASS DECLARATION {{{1
################################################################

BiodbObserver <- methods::setRefClass("BiodbObserver", fields = list())

# CONSTANTS {{{1
################################################################

MSG.INFO <- 'INFO'
MSG.DEBUG <- 'DEBUG'
MSG.WARNING <- 'WARNING'
MSG.ERROR <- 'ERROR'

.MSG.TYPES <- c(MSG.ERROR, MSG.WARNING, MSG.DEBUG, MSG.INFO)

# MESSAGE {{{1
################################################################

BiodbObserver$methods( message = function(type = MSG.INFO, msg, class = NA_character_, method = NA_character_, level = 1) {
})
