# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbObserver {{{1
################################################################################

# Declaration {{{2
################################################################################

#' The mother abstract class of all observer classes.
#'
#' This abstract class defines all the methods that can be used to send messages
#' to the observers. You can define new observer classes by inherting from this
#' class.
#'
#' @seealso Sub-classes \code{\link{BiodbLogger}},
#' \code{\link{BiodbInfoReporter}}, \code{\link{BiodbWarningReporter}},
#' \code{\link{BiodbErrorReporter}}.
#'
#' @examples
#' # Define a new observer class
#' MyObsClass <- methods::setRefClass("MyObsClass", contains='BiodbObserver',
#'   methods=list(message=function(type='info', msg,
#'                                 class=NA_character_,
#'                                 method=NA_character_, lvl=1) {
#'     .self$checkMessageType(type)
#'     print(paste(type, msg, sep=': '))
#'   }
#' ))
#'
#' # Create an instance and register an instance of the new observer class:
#' mybiodb <- biodb::Biodb()
#' mybiodb$addObservers(MyObsClass$new())
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @export BiodbObserver
#' @exportClass BiodbObserver
BiodbObserver <- methods::setRefClass("BiodbObserver",
    fields=list(
        cfg.lvl='integer',
        cust.lvl='integer',
        .lastime.progress='list',
        .progress.initial.time='list'
    ),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function() {
    .self$cfg.lvl <- integer()
    .self$cust.lvl <- integer()
    .self$.lastime.progress <- list()
    .self$.progress.initial.time <- list()
},

# Terminate {{{3
################################################################################

terminate=function() {
    ":\n\nTerminates the instance. This method will be called
    automatically by the Biodb instance when you call Biodb::terminate().
    \nReturned value: None.
    "

    invisible()
},

# Show {{{3
################################################################################

show=function() {
    ":\n\nDisplays information about this observer instance.
    \nReturned valued: none.
    "

    cat('Observer of class ', class(.self), ".\n", sep='')
},

# New observer {{{3
################################################################################

newObserver=function(obs) {
    ":\n\nCalled by Biodb when a new observer is registered.
    \nobs: The observer newly registered by the Biodb instance.
    \nReturned valued: none.
    "
},

# Config key value update {{{3
################################################################################

cfgKVUpdate=function(k, v) {
    ":\n\nThis method is called by BiodbConfig when the value of one of its keys
    is modified.
    \nk: The configuration key name.
    \nv: The new value of the configuration key.
    \nReturned value: None.
    "

    for (type in c('debug', 'info', 'caution')) {
        lvl.k <- paste('msg', type, 'lvl', sep='.')
        if (lvl.k == k)
            .self$cfg.lvl[[type]] <- v
    }
},

# Get level {{{3
################################################################################

getLevel=function(type) {
    ":\n\nGets the level associated with a message type. This is the maximum
    level a message must have in order to be processed.
    \ntype: The type of message. It must be one of: 'info', 'debug',
    'caution', 'warning', 'error'.
    \nReturned value: The level, as an integer.
    "

    lvl <- 0L
    .self$checkMessageType(type)

    if (type %in% names(.self$cust.lvl))
        lvl <- .self$cust.lvl[[type]]
    else if (type %in% names(.self$cfg.lvl))
        lvl <- .self$cfg.lvl[[type]]

    return(lvl)
},

# Set level {{{3
################################################################################

setLevel=function(type, lvl) {
    ":\n\nSets the level for a type. This is the maximum level a message must
    have in order to be processed.
    \ntype: The type of message. It must be one of: 'info', 'debug',
    'caution', 'warning', 'error'.
    \nlvl: The level, as an integer.
    \nReturned value: None.
    "

    .self$checkMessageType(type)
    .self$cust.lvl[[type]] <- as.integer(lvl)
},

# Message {{{3
################################################################################

msg=function(type='info', msg, class=NA_character_,
             method=NA_character_, lvl=1) {
    ":\n\nSends a message to this observer. The message will be accepted and
    handled only if the level is lower or equal to the current level for the
    message type. You can check current level for a type by calling getLevel()
    and set the level with setLevel().
    \ntype: The message type. It must be one of: 'info', 'debug',
    'caution', 'warning', 'error'.
    \nmsg: The text message to send.
    \nclass: The class of the object that called this message method.
    \nmethod: The method that called this message method.
    \nlvl: The level of the message.
    \nReturned value: None.
    "

    .self$checkMessageType(type)
},

# Info progress {{{3
################################################################################

progress=function(type='info', msg, index, first, total=NA_integer_, lvl=1L,
                  laptime=10L) {
    ":\n\nSends a progress message to this observer.
    \ntype: The message type. It must be one of: 'info', 'debug',
    'caution', 'warning', 'error'.
    \nmsg: The text message to send.
    \nindex: The index in the progression, as an integer or numeric number.
    \ntotal: The total to achieve in the progression, as an integer or numeric
    number. Optional.
    \nlvl: The level of the message.
    \nlaptime: The time between two progress messages, in seconds.
    \nReturned value: None.
    "

    t <- Sys.time()
    msgid <- msg

    if (first || ! msg %in% names(.self$.lastime.progress)) {
        .self$.lastime.progress[[msgid]] <- t
        .self$.progress.initial.time[[msgid]] <- t
    }

    if (t - .self$.lastime.progress[[msgid]] >= laptime) {
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

    invisible(NULL)
},

# Check message type {{{3
################################################################################

checkMessageType=function(type) {
    ":\n\nChecks a message type. An error will be raised if the type is unknown.
    \ntype: A message type. It must be one of: 'info', 'debug', 'caution',
    'warning', 'error'.
    \nReturned value: None.
    "

    # Define allowed types
    allowed.types <- c('info', 'debug', 'caution', 'warning', 'error')

    # Is type unknown?
    if ( ! tolower(type) %in% allowed.types)
        stop("Unknown message type \"", type, "\". Please use one of: ",
             paste(allowed.types, collapse=', '), '.')
}

))
