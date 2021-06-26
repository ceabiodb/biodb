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
}
))
