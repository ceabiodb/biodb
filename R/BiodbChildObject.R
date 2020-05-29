#' @include BiodbObject.R
BiodbChildObject <- methods::setRefClass("BiodbChildObject",
    contains='BiodbObject',
    fields=list(
        .parent="ANY"
    ),

methods=list(

initialize=function(parent, ...) {

    callSuper(...)
    .self$.abstractClass('BiodbChildObject')
    .self$.setParent(parent)
},

getParent=function() {
    return(.self$.parent)
},

getBiodb=function() {
    return(.self$getParent()$getBiodb())
},

.setParent=function(parent) {

    if (is.null(parent))
        .self$message('error', "Parent cannot be NULL.")
    if ( ! is(parent, 'BiodbObject'))
        .self$message('error', "Parent must inherit from BiodbObject.")
    .self$.parent <- parent
}

))
