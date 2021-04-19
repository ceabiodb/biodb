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
