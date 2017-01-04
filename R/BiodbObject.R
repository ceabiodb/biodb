# vi: fdm=marker

# CLASS DECLARATION {{{1
################################################################

BiodbObject <- methods::setRefClass("BiodbObject", fields = list( .observers = "ANY", .message.enabled = "logical"))

# CONSTRUCTOR {{{1
################################################################

BiodbObject$methods( initialize = function(...) {
	.message.enabled <<- TRUE
	callSuper(...)
})

# ABSTRACT METHOD {{{1
################################################################

# This method is used to declare a method as abstract.
BiodbObject$methods( .abstract.method = function() {

	class <- class(.self)
	method <- sys.call(length(sys.calls()) - 1)
	method <- sub('^[^$]*\\$([^(]*)(\\(.*)?$', '\\1()', method)

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
BiodbObject$methods( message = function(type, msg, level = 1) {

	# Get biodb instance
	biodb <- NULL
	if (.self$.message.enabled) {
		.message.enabled <<- FALSE
		biodb <- .self$getBiodb() 
		.message.enabled <<- TRUE
	}

	if ( ! is.null(biodb))
		lapply(biodb$getObservers(), function(x) x$message(type = type, msg = msg, class = class(.self), level = level))
	else {
		class.info <- if (is.na(class(.self))) '' else paste0('[', class(.self), '] ')
		switch(type,
		       ERROR = stop(paste0(class.info, msg)),
		       WARNING = warning(paste0(class.info, msg)),
		       cat(class.info, msg, "\n", file = stderr()))
	}
})
