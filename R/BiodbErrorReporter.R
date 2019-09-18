# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbErrorReporter {{{1
################################################################################

# Declaration {{{2
################################################################################

#' A class for throwing an error.
#'
#' This class is not meant to be used directly. It is automatically instantiated
#' inside biodb constructor in order to report errors.
#'
#' @seealso \code{\link{Biodb}}, \code{\link{BiodbObserver}}.
#'
#' @import methods
#' @include BiodbObserver.R
BiodbErrorReporter <- methods::setRefClass("BiodbErrorReporter",
    contains='BiodbObserver',

# Public methods {{{2
################################################################################

methods=list(

# Message {{{3
################################################################################

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

# Info progress {{{3
################################################################################

progress=function(type='info', msg, index, first, total=NA_integer_, lvl=1L,
                  laptime=10L) {
    # Override super class' method

    invisible(NULL)
}

))
