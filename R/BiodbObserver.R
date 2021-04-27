#' The mother abstract class of all observer classes.
#'
#' This abstract class defines all the methods that can be used to send messages
#' to the observers. You can define new observer classes by inherting from this
#' class.
#'
#' @examples
#' # Define a new observer class
#' MyObsClass <- methods::setRefClass("MyObsClass", contains='BiodbObserver',
#'   methods=list(notifyProgress=function(what, index, total) {
#'       sprintf("Progress for %s is %d / %d.", what, index, total)
#'   }
#' ))
#'
#' # Create an instance and register an instance of the new observer class:
#' mybiodb <- biodb::newInst()
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
    ),

methods=list(

initialize=function() {
},

terminate=function() {
    ":\n\nTerminates the instance. This method will be called
    automatically by the BiodbMain instance when you call
    BiodbMain::terminate().
    \nReturned value: None.
    "

    invisible()
},

show=function() {
    ":\n\nDisplays information about this observer instance.
    \nReturned valued: none.
    "

    cat('Observer of class ', class(.self), ".\n", sep='')
},

newObserver=function(obs) {
    ":\n\nCalled by BiodbMain when a new observer is registered.
    \nobs: The observer newly registered by the BiodbMain instance.
    \nReturned valued: none.
    "
},

cfgKVUpdate=function(k, v) {
    ":\n\nThis method is called by BiodbConfig when the value of one of its keys
    is modified.
    \nk: The configuration key name.
    \nv: The new value of the configuration key.
    \nReturned value: None.
    "
},

notifyProgress=function(what, index, total) {
    ":\n\nNotify about the progress of an action.
    \nwhat: A short description of the action.
    \nindex: A positive integer number indicating the progress.
    \ntotal: The maximum for \"index\". When reached, the action is completed.
    \nReturned value: None.
    "
    chk::chk_string(what)
    chk::chk_whole_number(index)
    chk::chk_whole_number(total)
    chk::chk_gte(index, 0)
    chk::chk_lte(index, total)
    
    return(invisible(NULL))
}

))
