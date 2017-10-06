# vi: fdm=marker

#' @include MassbankConn.R

# Constants {{{1
################################################################

MASSBANK.EU.URL  <- 'http://massbank.eu/'

# Class declaration {{{1
################################################################

MassbankEuConn <- methods::setRefClass("MassbankEuConn", contains = 'MassbankConn')

