# vi: fdm=marker

# CLASS DECLARATION {{{1
################################################################

BiodbObject <- methods::setRefClass("BiodbObject", fields = list( .observers = "ANY" ))

# ABSTRACT METHOD {{{1
################################################################

# This method is used to declare a method as abstract.
BiodbObject$methods( .abstract.method = function() {

	class <- class(.self)
	method <- sys.call(length(sys.calls()) - 1)
	method <- sub('^[^$]*\\$([^(]*)\\(.*$', '\\1()', method)

	.self$message(type = MSG.ERROR, paste("Method", method, "is not implemented in", class, "class."))
})

# GET BIODB {{{1
################################################################

BiodbObject$methods( getBiodb = function() {
	.self$.abstract.method()
})

# MESSAGE {{{1
################################################################

# Send a message to observers
BiodbObject$methods( message = function(type = MSG.INFO, msg, level = 1) {
	lapply(.self$getBiodb()$getObservers(), function(x) x$message(type = type, msg = msg, level = level))
})
