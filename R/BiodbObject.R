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

# GET ENV VAR {{{1
################################################################

BiodbObject$methods( getEnvVar = function(name) {

	biodb <- .self$getBiodb() 

	if ( ! is.null(biodb))
		return(biodb$getEnvVar(name))

	return(NA_character_)
})

# GET USER AGENT {{{1
################################################################

BiodbObject$methods( getUserAgent = function() {

	biodb <- .self$getBiodb() 

	if ( ! is.null(biodb))
		return(biodb$getUserAgent())

	return(NA_character_)
})

# MESSAGE {{{1
################################################################

# Send a message to observers
BiodbObject$methods( message = function(type = MSG.INFO, msg, level = 1) {

	biodb <- .self$getBiodb() 

	if ( ! is.null(biodb))
		lapply(biodb$getObservers(), function(x) { print(class(x)); x$message(type = type, msg = msg, class = class(.self), level = level) })
	else {
		class.info <- if (is.na(class)) '' else paste0('[', class, '] ')
		switch(type,
		       ERROR = stop(paste0(classinfo, msg)),
		       WARNING = warning(paste0(classinfo, msg)),
		       cat(classinfo, msg, "\n", file = stderr()))
	}
})
