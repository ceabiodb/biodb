#' A class for display info messages for the user.
#'
#' This class is not meant to be used directly. It is automatically instantiated
#' inside biodb constructor in order to report information messages.
#'
#' @seealso \code{\link{Biodb}} and super class \code{\link{BiodbObserver}}.
#'
#' @import methods
#' @include BiodbObserver.R
BiodbInfoReporter <- methods::setRefClass("BiodbInfoReporter",
    contains='BiodbObserver',

methods=list(

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
