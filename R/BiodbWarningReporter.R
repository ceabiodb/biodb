# vi: fdm=marker ts=4 et cc=80

# CLASS DECLARATION {{{1
################################################################################

#' A class for logging warning messages.
#'
#' This class is not meant to be used directly. It is automatically instantiate inside biodb constructor in order to report warnings.
#'
#' @seealso \code{\link{Biodb}}, \code{\link{BiodbObserver}}.
#'
#' @import methods
#' @include BiodbObserver.R
#' @export BiodbWarningReporter
#' @exportClass BiodbWarningReporter
BiodbWarningReporter <- methods::setRefClass("BiodbWarningReporter", contains='BiodbObserver')

# MESSAGE {{{1
################################################################################

BiodbWarningReporter$methods( msg=function(type='info', msg, class=NA_character_, method=NA_character_, lvl=1) {

    .self$checkMessageType(type)

    # Raise warning
    if (type == 'warning') {
        caller.info <- if (is.na(class)) '' else class
        caller.info <- if (is.na(method)) caller.info else paste(caller.info, method, sep='::')
        if (nchar(caller.info) > 0) caller.info <- paste('[', caller.info, '] ', sep='')
        warning(paste0(caller.info, msg))
    }
})
