# vi: fdm=marker

#' @include BiodbObject.R

# Class declaration {{{1
################################################################

BiodbChildObject <- methods::setRefClass("BiodbChildObject", contains = 'BiodbObject', fields = list( .parent = "ANY" ))

# Constructor {{{1
################################################################

BiodbChildObject$methods( initialize = function(parent, ...) {

	callSuper(...)
	.self$.abstract.class('BiodbChildObject')
	.self$.setParent(parent)
})

# Get parent {{{1
################################################################

BiodbChildObject$methods( getParent = function() {
	return(.self$.parent)
})

# Get biodb {{{1
################################################################

BiodbChildObject$methods( getBiodb = function() {
	return(.self$getParent()$getBiodb())
})

# Private methods {{{1
################################################################

BiodbChildObject$methods( .setParent = function(parent) {

	if (is.null(parent))
		.self$message('error', "Parent cannot be NULL.")
	if ( ! is(parent, 'BiodbObject'))
		.self$message('error', "Parent must inherit from BiodbObject.")
	.self$.parent <- parent
})
