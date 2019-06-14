# vi: fdm=marker ts=4 et cc=80

# BiodbLogger {{{1
################################################################################

#' A class for logging biodb messages either to standard stream or into a file.
#'
#' This class implements a logger for the biodb package. When creating an
#' instance of this class, you can choose to either log to a standard stream
#' (standard error or standard output) or to a file. See section Fields for a
#' list of the constructor's parameters.
#'
#' @field file The file path, file connection or standard stream to which you
#'        want to send biodb messages.
#' @field mode For a file, the mode in which you want to open the file. 'w' for
#'        writing (if a file already exists, it will be erased), and 'a' for
#'        appending (if a file already exists, logs will be appended to it).
#'
#' @seealso \code{\link{Biodb}}, \code{\link{BiodbObserver}}.
#'
#' @examples
#' # Create a file logger
#' logger <- biodb::BiodbLogger(file="myfile.log")
#'
#' # Create a biodb instance
#' mybiodb <- biodb::Biodb()
#'
#' # Add the logger to the list of observers
#' mybiodb$addObservers(logger)
#'
#' # Terminate instance.
#' mybiodb$terminate()
#' 
#' @import methods
#' @include BiodbObserver.R
#' @export BiodbLogger
#' @exportClass BiodbLogger
BiodbLogger <- methods::setRefClass("BiodbLogger",
                                    contains='BiodbObserver',

# Fields {{{2
################################################################################

fields=list(
    .file='ANY',
    .close.file='logical',
    .lastime.progress='list',
    .progress.laptime='integer',
    .progress.initial.time='list'),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(file=NULL, mode='w', close.file=TRUE, ...) {

    callSuper(...)

    # Is file NA?
    if ( ! is.null(file) && (is.na(file) || ( ! is.character(file)
                             && ! methods::is(file, 'connection'))))
        stop("You must choose either a connection to a file or a file path",
              " for the \"file\" parameter")

    # File is a path => create a connection
    if (is.character(file))
        file <- file(file, open=mode)

    # Check file class type
    if ( ! is.null(file) && ! methods::is(file, 'file')
        && ! methods::is(file, 'terminal'))
        stop('Unknown class "', class(file), '" for log file.')

    # Set member field
    .self$.file <- file
    .self$.close.file <- close.file
    .self$.lastime.progress <- list()
    .self$.progress.initial.time <- list()
    .self$.progress.laptime <- as.integer(10) # In seconds
},

# Terminate {{{3
################################################################################

terminate=function() {
    if (.self$.close.file && ! is.null(.self$.file)
        && ! methods::is(.self$.file, 'terminal'))
        close(.self$.file)
},

# Message {{{3
################################################################################

msg=function(type='info', msg, class=NA_character_,
                   method=NA_character_, lvl=1) {

    .self$checkMessageType(type)
    setlvl <- .self$getLevel(type)

    if (setlvl >= lvl) {

        # Set caller information
        caller.info <- if (is.na(class)) '' else class
        caller.info <- if (is.na(method)) caller.info
            else paste(caller.info, method, sep='::')
        if (nchar(caller.info) > 0)
            caller.info <- paste('[', caller.info, '] ', sep='')

        # Set timestamp
        timestamp <- paste('[', as.POSIXlt(Sys.time()), ']', sep='')

        # Output message
        m <- paste0('BIODB.', toupper(type), timestamp, caller.info, ': ', msg)
        if (is.null(.self$.file))
            base::message(m)
        else
            cat(m, "\n", sep='', file=.self$.file)
    }
},

# Info progress {{{3
################################################################################

progress=function(type='info', msg, index, first, total=NA_integer_, lvl=1) {
    
    t <- Sys.time()
    msgid <- msg

    if (first || ! msg %in% names(.self$.lastime.progress)) {
        .self$.lastime.progress[[msgid]] <- t
        .self$.progress.initial.time[[msgid]] <- t
    }

    else if (t - .self$.lastime.progress[[msgid]] > .self$.progress.laptime) {
        msg <- paste(msg, index, '/', if (is.na(total)) '?' else total)
        if ( ! is.na(total)) {
            i <- .self$.progress.initial.time[[msgid]]
            eta <- t + (total - index) * (t - i) / index
            msg <- paste0(msg, ' (', ((100 * index) %/% total), '%, ETA: ',
                          eta, ')')
        }
        msg <- paste0(msg, '.')
        .self$msg(type, msg, lvl=lvl)
        .self$.lastime.progress[[msgid]] <- t
    }
}

))
