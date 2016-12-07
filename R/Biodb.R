# vi: fdm=marker

# CLASS DECLARATION {{{1
################################################################

Biodb <- methods::setRefClass("Biodb", contains = "BiodbObject", fields = list( .observers = "ANY" ))

# CONSTRUCTOR {{{1
################################################################

Biodb$methods( initialize = function(...) {

	.observers <<- list(WarningReporter$new(), ErrorReporter$new())

	callSuper(...)
})

# ADD OBSERVERS {{{1
################################################################

Biodb$methods( addObservers = function(obs) {

	# Check types of observers
	if ( ( ! is.list(obs) && ! inherits(obs, "BiodbObserver")) || (is.list(obs) && any( ! vapply(obs, function(o) inherits(o, "BiodbObserver"), FUN.VALUE = TRUE))))
		stop("Observers must inherit from BiodbObserver class.")

	# Add observers to current list (insert at beginning)
	.observers <<- if (is.null(.self$.observers)) c(obs) else c(obs, .self$.observers)
})

# GET OBSERVERS {{{1
################################################################

Biodb$methods( getObservers = function() {
	return(.self$.observers)
})
