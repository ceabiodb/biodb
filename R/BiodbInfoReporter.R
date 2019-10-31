# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbInfoReporter {{{1
################################################################################

# Declaration {{{2
################################################################################

#' A class for display info messages for the user.
#'
#' This class is not meant to be used directly. It is automatically instantiated
#' inside biodb constructor in order to report information messages.
#'
#' @seealso \code{\link{Biodb}}, \code{\link{BiodbObserver}}.
#'
#' @import methods
#' @include BiodbObserver.R
BiodbInfoReporter <- methods::setRefClass("BiodbInfoReporter",
    contains='BiodbObserver',

# Public methods {{{2
################################################################################

methods=list(

# Message {{{3
################################################################################

msg=function(type='info', msg, class=NA_character_, method=NA_character_,
             lvl=1) {

    .self$checkMessageType(type)
    setlvl <- .self$getLevel(type)

    if (setlvl >= lvl && type %in% c('info', 'debug', 'caution'))
        base::message(toupper(substr(type, 1, 1)), substr(type, 2, nchar(type)),
                      ' message: ', msg)

    invisible()
}

))
