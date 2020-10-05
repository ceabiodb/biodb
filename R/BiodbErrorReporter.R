#' A class for throwing an error.
#'
#' This class is not meant to be used directly. It is automatically instantiated
#' inside biodb constructor in order to report errors.
#'
#' @seealso \code{\link{Biodb}} and super class \code{\link{BiodbObserver}}.
#'
#' @import methods
#' @include BiodbObserver.R
BiodbErrorReporter <- methods::setRefClass("BiodbErrorReporter",
    contains='BiodbObserver',

methods=list(

msg=function(type='info', msg, class=NA_character_, method=NA_character_,
             lvl=1) {
    # Override super class' method

    .self$checkMessageType(type)

    # Raise error
    if (type == 'error') {
        caller.info <- if (is.na(class)) '' else class
        caller.info <- if (is.na(method)) caller.info
            else paste(caller.info, method, sep='::')
        if (nchar(caller.info) > 0)
            caller.info <- paste('[', caller.info, '] ', sep='')
        stop(paste0(caller.info, msg))
    }

    invisible(NULL)
},

progress=function(type='info', msg, index, first, total=NA_integer_, lvl=1L,
                  laptime=10L) {
    # Override super class' method

    invisible(NULL)
}

))
