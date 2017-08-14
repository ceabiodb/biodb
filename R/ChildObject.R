# vi: fdm=marker

#' @include BiodbObject.R

# Class declaration {{{1
################################################################

ChildObject <- methods::setRefClass("ChildObject", contains = 'BiodbObject', fields = list( .parent = "ANY" ))

# Constructor {{{1
################################################################

ChildObject$methods( initialize = function(parent, ...) {

	callSuper(...)

	# Set parent
	if (is.null(parent))
		.self$message('error', "Parent cannot be NULL.")
	if ( ! is(parent, 'BiodbObject'))
		.self$message('error', "Parent must inherit from BiodbObject.")
	.parent <<- parent
})

# Get parent {{{1
################################################################

ChildObject$methods( getParent = function() {
	return(.self$.parent)
})

# Get biodb {{{1
################################################################

ChildObject$methods( getBiodb = function() {
	return(.self$getParent()$getBiodb())
})
