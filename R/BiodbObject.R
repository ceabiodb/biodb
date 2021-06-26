BiodbObject <- methods::setRefClass("BiodbObject",
    fields=list(
                ),

methods=list(

initialize=function() {

    abstractClass('BiodbObject', .self)
},

getBiodb=function() {
    .self$abstract.method()
},

help=function() {
    utils::help(class(.self), 'biodb')
},

# This method is used to declare a method as abstract.
.abstractMethod=function() {

    class <- class(.self)
    method <- sys.calls()[[length(sys.calls()) - 1]]
    method <- as.character(method)
    method <- method[[1]]
    method <- sub('^[^$]*\\$([^(]*)(\\(.*)?$', '\\1()', method)

    error0("Method ", method, " is not implemented in ", class, " class.")
}
))
