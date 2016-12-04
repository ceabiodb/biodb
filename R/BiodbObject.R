##########################
# CLASS DECLARATION {{{1 #
##########################

BiodbObject <- methods::setRefClass("BiodbObject", fields = list( .observers = "ANY" ))

########################
# ABSTRACT METHOD {{{1 #
########################

BiodbObject$methods( .abstract.method = function() {

	class <- class(.self)
	method <- sys.call(length(sys.calls()) - 1)
	method <- sub('^[^$]*\\$([^(]*)\\(.*$', '\\1()', method)

	stop(paste("Method", method, "is not implemented in", class, "class."))
})

######################
# ADD OBSERVERS {{{1 #
######################

BiodbObject$methods( addObservers = function(obs) {

	# Check types of observers
	if ( ( ! is.list(obs) && ! inherits(obs, "BiodbObserver")) || (is.list(obs) && any( ! vapply(obs, function(o) inherits(o, "BiodbObserver"), FUN.VALUE = TRUE))))
		stop("Observers must inherit from BiodbObserver class.")

	# Add observers to current list
	.observers <<- if (is.null(.self$.observers)) c(obs) else c(.self$.observers, obs)
})
