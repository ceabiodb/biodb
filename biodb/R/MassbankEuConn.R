# vi: fdm=marker

#' @include MassbankConn.R

# Constants {{{1
################################################################

MASSBANK.EU.URL  <- 'http://massbank.eu/'

# Class declaration {{{1
################################################################

MassbankEuConn <- methods::setRefClass("MassbankEuConn", contains = 'MassbankConn')

# Constructor {{{1
################################################################0

MassbankEuConn$methods( initialize = function(...) {

	callSuper(base.url = MASSBANK.EU.URL, ...)
})
