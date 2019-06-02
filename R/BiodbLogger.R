# vi: fdm=marker ts=4 et cc=80

# BiodbLogger {{{1
################################################################################

#' A class for logging biodb messages either to standard stream or into a file.
#'
#' This class implements a logger for the biodb package. When creating an instance of this class, you can choose to either log to a standard stream (standard error or standard output) or to a file. See section Fields for a list of the constructor's parameters.
#'
#' @field file The file path, file connection or standard stream to which you want to send biodb messages.
#' @field mode For a file, the mode in which you want to open the file. 'w' for writing (if a file already exists, it will be erased), and 'a' for appending (if a file already exists, logs will be appended to it).
#'
#' @seealso \code{\link{Biodb}}, \code{\link{BiodbObserver}}.
#'
#' @examples
#' # Create a biodb instance with a file log
#' mybiodb <- biodb::Biodb(observers = biodb::BiodbLogger(file = "myfile.log"))
#'
#' # Terminate instance.
#' mybiodb$terminate()
#' 
#' # Create a biodb instance with logging to standard output
#' mybiodb <- biodb::Biodb(observers = biodb::BiodbLogger(file = stdout()))
#'
#' # Terminate instance.
#' mybiodb$terminate()
#' 
#' @import methods
#' @include BiodbObserver.R
#' @export BiodbLogger
#' @exportClass BiodbLogger
BiodbLogger <- methods::setRefClass("BiodbLogger",
                                    contains = 'BiodbObserver',

# Fields {{{2
################################################################################

fields = list(
    .file = 'ANY',
    .close.file = 'logical',
    .levels = 'integer',
    .lastime.progress = 'list',
    .progress.laptime = 'integer',
    .progress.initial.time = 'list'),

# Public methods {{{2
################################################################################

methods = list(

# Initialize {{{3
################################################################################

initialize = function(file = NULL, mode = 'w', close.file = TRUE, ...) {

    callSuper(...)

    # Is file NA?
    if ( ! is.null(file) && (is.na(file) || ( ! is.character(file)
                             && ! methods::is(file, 'connection'))))
        stop("You must choose either a connection to a file or a file path",
              " for the \"file\" parameter")

    # File is a path => create a connection
    if (is.character(file))
        file <- file(file, open = mode)

    # Check file class type
    if ( ! is.null(file) && ! methods::is(file, 'file')
        && ! methods::is(file, 'terminal'))
        stop('Unknown class "', class(file), '" for log file.')

    # Set member field
    .self$.file <- file
    .self$.close.file <- close.file
    .self$.levels <- integer()
    .self$.lastime.progress <- list()
    .self$.progress.initial.time <- list()
    .self$.progress.laptime <- as.integer(10) # In seconds
},

# Terminate {{{3
################################################################################

terminate = function() {
    if (.self$.close.file && ! is.null(.self$.file)
        && ! methods::is(.self$.file, 'terminal'))
        close(.self$.file)
},

# Config key value set {{{3
################################################################################

cfgKeyValSet = function(k, v) {
    for (type in c('debug', 'info', 'caution')) {
        lvl.k <- paste('msg', type, 'lvl', sep='.')
        if (lvl.k == k)
            .self$.levels[[k]] <- v
    }
},

# Get desired level {{{3
################################################################################

getDesiredLevel = function(type) {

    lvl <- 1
    
    if (type %in% names(.self$.levels))
        lvl <- .self$.levels[[type]]
    
    return(lvl)
},

# Message {{{3
################################################################################

message = function(type = 'info', msg, class = NA_character_, method = NA_character_) {

    .self$checkMessageType(type)
    setlvl <- .self$getDesiredLevel(type)

    if (setlvl >= 1) {

        # Set caller information
        caller.info <- if (is.na(class)) '' else class
        caller.info <- if (is.na(method)) caller.info else paste(caller.info, method, sep = '::')
        if (nchar(caller.info) > 0) caller.info <- paste('[', caller.info, '] ', sep = '')

        # Set timestamp
        timestamp <- paste('[', as.POSIXlt(Sys.time()), ']', sep = '')

        # Output message
        m <- paste0('BIODB.', toupper(type), timestamp, caller.info, ': ', msg)
        if (is.null(.self$.file))
            base::message(m)
        else
            cat(m, "\n", sep = '', file = .self$.file)
    }
},

# Info progress {{{3
################################################################################

progress = function(type = 'info', msg, index, total, first) {

    if (first || ! msg %in% names(.self$.lastime.progress))
        .self$.lastime.progress[[msg]] = .self$.progress.initial.time[[msg]] = Sys.time()

    else if (Sys.time() - .self$.lastime.progress[[msg]] > .self$.progress.laptime) {
        eta = Sys.time() + (total - index) * (Sys.time() - .self$.progress.initial.time[[msg]]) / index
        .self$message(type, paste0(msg, ' ', index, ' / ', total, ' (', ((100 * index) %/% total), '%, ETA: ', eta, ').'))
        .self$.lastime.progress[[msg]] = Sys.time()
    }
}

))
