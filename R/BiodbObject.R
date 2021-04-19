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

info=function(...) {
    .self$message(type='info', msg=paste0(...))

    invisible()
},

# This method is used to declare a class as abstract.
.abstractClass=function(class) {

    if (class == class(.self))
        fatal('Class ', class, ' is abstract and thus cannot be ',
                    'instantiated.', fmt='paste0')
},

# This method is used to declare a method as abstract.
.abstractMethod=function() {

    class <- class(.self)
    method <- sys.calls()[[length(sys.calls()) - 1]]
    method <- as.character(method)
    method <- method[[1]]
    method <- sub('^[^$]*\\$([^(]*)(\\(.*)?$', '\\1()', method)

    fatal("Method ", method, " is not implemented in ", class, " class.",
          fmt='paste0')
}
))
