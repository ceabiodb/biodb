#' A class for throwing a warning.
#'
#' This class is not meant to be used directly. It is automatically instantiated
#' inside biodb constructor in order to report warnings.
#'
#' @seealso \code{\link{Biodb}} and super class \code{\link{BiodbObserver}}.
#'
#' @import methods
#' @include BiodbObserver.R
BiodbWarningReporter <- methods::setRefClass("BiodbWarningReporter",
    contains='BiodbObserver',

methods=list(

msg=function(type='info', msg, class=NA_character_, method=NA_character_,
             lvl=1) {
    # Override super class' method

    .self$checkMessageType(type)

    # Raise warning
    if (type == 'warning') {
        caller.info <- if (is.na(class)) '' else class
        caller.info <- if (is.na(method)) caller.info
            else paste(caller.info, method, sep='::')
        if (nchar(caller.info) > 0)
            caller.info <- paste('[', caller.info, '] ', sep='')
        warning(paste0(caller.info, msg))
    }

    invisible(NULL)
}

))
