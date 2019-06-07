# vi: fdm=marker ts=4 et cc=80

# BiodbObserver {{{1
################################################################################

#' The mother abstract class of all observer classes.
#'
#' This abstract class defines all the methods that can be used to send messages
#' to the observers. You can define new observer classes by inherting from this
#' class.
#'
#' @param type   The type of a message. It must be one of: 'info', 'debug',
#'               'caution', 'warning', 'error'.
#' @param msg    The text message to send.
#' @param class  The class of the object that called this message method.
#' @param method The method of that called this message method.
#'
#' @seealso \code{\link{BiodbLogger}}, \code{\link{BiodbWarningReporter}},
#'          \code{\link{BiodbErrorReporter}}.
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

# Fields {{{2
################################################################################

fields=list(
    cfg.lvl='integer',
    cust.lvl='integer'
),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function() {
    .self$cfg.lvl <- integer()
    .self$cust.lvl <- integer()
},

# Terminate {{{3
################################################################################

terminate=function() {
},

# Show {{{3
################################################################################

show=function() {
    cat('Observer of class ', class(.self), ".\n", sep='')
},

# New observer {{{3
################################################################################

newObserver=function(obs) {
},

# Config key value update {{{3
################################################################################

cfgKVUpdate=function(k, v) {
    for (type in c('debug', 'info', 'caution')) {
        lvl.k <- paste('msg', type, 'lvl', sep='.')
        if (lvl.k == k)
            .self$cfg.lvl[[type]] <- v
    }
},

# Get level {{{3
################################################################################

getLevel=function(type) {

    lvl <- 0
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
    .self$checkMessageType(type)
    .self$cust.lvl[[type]] <- as.integer(lvl)
},

# Message {{{3
################################################################################

msg=function(type='info', msg, class=NA_character_,
             method=NA_character_, lvl=1) {
    .self$checkMessageType(type)
},

# Info progress {{{3
################################################################################

progress=function(type='info', msg, index, total, first, lvl=1) {
    .self$checkMessageType(type)
},

# Check message type {{{3
################################################################################

checkMessageType=function(type) {

    # Define allowed types
    allowed.types <- c('info', 'debug', 'caution', 'warning', 'error')

    # Is type unknown?
    if ( ! tolower(type) %in% allowed.types)
        stop("Unknown message type \"", type, "\". Please use one of: ",
             paste(allowed.types, collapse=', '), '.')
}

))
