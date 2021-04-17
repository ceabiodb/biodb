BiodbObject <- methods::setRefClass("BiodbObject",
    fields=list(
                .message.enabled="logical"
                ),

methods=list(

initialize=function() {

    .self$.abstractClass('BiodbObject')

    .self$.message.enabled <- TRUE
},

getBiodb=function() {
    .self$abstract.method()
},

help=function() {
    utils::help(class(.self), 'biodb')
},

notify=function(fct, args) {

    # Get observers
    obs <- .self$getBiodb()$getObservers()

    # Build call code
    call <- paste0('do.call(o$', fct, ', args)')

    # Notify each observer
    lapply(obs, function(o) eval(parse(text=call)) )

    invisible()
},

progressMsg=function(msg, index, first, total=NA_integer_, type='info',
                     laptime=10L) {
    .self$notify('progress', list(type=type, msg=msg, index=index, total=total,
                                  first=first, laptime=laptime))

    invisible()
},

message=function(type, msg, lvl=1, callerLvl=0) {
# Send a message to observers

    type <- tolower(type)

    # Get biodb instance
    biodb <- NULL
    if (length(.self$.message.enabled) == 1 # length is 0
                                            # if called from constructor
        && .self$.message.enabled) {
        .self$.message.enabled <- FALSE
        biodb <- .self$getBiodb() 
        .self$.message.enabled <- TRUE
    }

    # Get class and method information
    class <- class(.self)
    method <- sys.call(sys.nframe() - (callerLvl + 1))
    method <- sub('^[^$]*\\$([^(]*)(\\(.*)?$', '\\1()', method)[[1]]

    if ( ! is.null(biodb))
        .self$notify('msg', list(type=type, msg=msg, class=class, method=method,
                                 lvl=lvl))
    else {
        caller.info <- if (is.na(class)) '' else class
        if (! is.na(method))
            caller.info <- paste(caller.info, method, sep='::')
        if (nchar(caller.info) > 0)
            caller.info <- paste('[', caller.info, '] ', sep='')
        switch(type,
               error=stop(caller.info, msg),
               warning=warning(caller.info, msg),
               base::message(caller.info, msg))
    }

    invisible()
},

debug=function(...) {
    .self$message(type='debug', msg=paste0(...))

    invisible()
},

debug2=function(..., callerLvl=0) {
    .self$message(type='debug', msg=paste0(...), lvl=2, callerLvl=callerLvl+1)

    invisible()
},

debug2Dataframe=function(msg, x, rowCut=5, colCut=5) {

    .self$debug2(msg, df2str(x, rowCut=rowCut, colCut=colCut), '.')

    return(invisible(NULL))
},

debug2List=function(msg, lst, nCut=10, callerLvl=0) {

    .self$debug2(msg, lst2str(lst, nCut=nCut), '.', callerLvl=callerLvl+1)

    return(invisible(NULL))
},

error=function(...) {
    .self$message(type='error', msg=paste0(...))

    invisible()
},

warning=function(...) {
    .self$message(type='warning', msg=paste0(...))

    invisible()
},

info=function(...) {
    .self$message(type='info', msg=paste0(...))

    invisible()
},

info2=function(...) {
    .self$message(type='info', msg=paste0(...), lvl=2)

    invisible()
},

# This method is used to declare a class as abstract.
.abstractClass=function(class) {

    if (class == class(.self))
        .self$error('Class ', class, ' is abstract and thus cannot be ',
                    'instantiated.')
},

# This method is used to declare a method as abstract.
.abstractMethod=function() {

    class <- class(.self)
    method <- sys.calls()[[length(sys.calls()) - 1]]
    method <- as.character(method)
    method <- method[[1]]
    method <- sub('^[^$]*\\$([^(]*)(\\(.*)?$', '\\1()', method)

    .self$error("Method ", method, " is not implemented in ", class, " class.")
}
))
