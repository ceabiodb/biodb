# vi: fdm=marker

#' @include MassbankConn.R

# Constants {{{1
################################################################

MASSBANK.JP.URL  <- 'http://www.massbank.jp/'

# Class declaration {{{1
################################################################

MassbankJpConn <- methods::setRefClass("MassbankJpConn", contains = c('MassbankConn'))

# Constructor {{{1
################################################################0

MassbankJpConn$methods( initialize = function(...) {

	callSuper(base.url = MASSBANK.JP.URL, ...)
})
