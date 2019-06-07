# vi: fdm=marker ts=4 et cc=80

# BiodbChildObject {{{1
################################################################################

#' @include BiodbObject.R
BiodbChildObject <- methods::setRefClass("BiodbChildObject",
    contains='BiodbObject',

# Fields {{{2
################################################################################
fields=list(
    .parent="ANY"
),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(parent, ...) {

    callSuper(...)
    .self$.abstractClass('BiodbChildObject')
    .self$.setParent(parent)
},

# Get parent {{{3
################################################################################

getParent=function() {
    return(.self$.parent)
},

# Get biodb {{{3
################################################################################

getBiodb=function() {
    return(.self$getParent()$getBiodb())
},

# Private methods {{{2
################################################################################

# Set parent {{{3
################################################################################

.setParent=function(parent) {

    if (is.null(parent))
        .self$message('error', "Parent cannot be NULL.")
    if ( ! is(parent, 'BiodbObject'))
        .self$message('error', "Parent must inherit from BiodbObject.")
    .self$.parent <- parent
}

))
