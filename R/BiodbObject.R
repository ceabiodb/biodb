# vi: fdm=marker

# Class declaration {{{1
################################################################

BiodbObject <- methods::setRefClass("BiodbObject", fields = list(.message.enabled = "logical"))

# Constructor {{{1
################################################################

BiodbObject$methods( initialize = function(...) {

	callSuper(...)
	.self$.abstract.class('BiodbObject')

	.message.enabled <<- TRUE
})

# Abstract class {{{1
################################################################

# This method is used to declare a class as abstract.
BiodbObject$methods( .abstract.class = function(class) {

	if (class == class(.self))
		.self$message(type = 'error', paste('Class', class, 'is abstract and thus cannot be instantiated.'))
})

# Abstract method {{{1
################################################################

# This method is used to declare a method as abstract.
BiodbObject$methods( .abstract.method = function() {

	class <- class(.self)
	method <- sys.calls()[[length(sys.calls()) - 1]]
	method <- as.character(method)
	method <- method[[1]]
	method <- sub('^[^$]*\\$([^(]*)(\\(.*)?$', '\\1()', method)

	.self$message(type = 'error', paste("Method", method, "is not implemented in", class, "class."))
})

# Deprecated method {{{1
################################################################

# This method is used to declare a method as deprecated.
BiodbObject$methods( .deprecated.method = function(new.method = NA_character_) {

	class <- class(.self)
	call <- sys.call(-1)
	call <- as.character(call)
	call <- call[[1]]
	calls <- strsplit(call, '$', fixed = TRUE)[[1]]
	method <- calls[[length(calls)]]

	msg <- paste("Method ", method, "() is now deprecated in ", class, " class.", sep = '')
	if ( ! is.na(new.method))
		msg <- paste(msg, " Please use now method ", new.method, ".", sep = '')
	.self$message('caution', msg)
})

# Assert not NA {{{1
################################################################

BiodbObject$methods( .assert.not.na = function(param, msg.type = 'error', sys.call.level = 0, param.name = '') {
	if (any(is.na(param))) {
		if (nchar(param.name) == 0)
			param.name <- as.character(sys.call(sys.call.level))[[2]]
		.self$message(msg.type, paste(param.name, ' cannot be set to NA.', sep = ''))
		return(FALSE)
	}
	return(TRUE)
})

# Assert not NULL {{{1
################################################################

BiodbObject$methods( .assert.not.null = function(param, msg.type = 'error', sys.call.level = 0, param.name = '') {
	if (is.null(param)) {
		if (nchar(param.name) == 0)
			param.name <- as.character(sys.call(sys.call.level))[[2]]
		.self$message(msg.type, paste(param.name, ' cannot be NULL.', sep = ''))
		return(FALSE)
	}
	return(TRUE)
})

# Assert inferior {{{1
################################################################

BiodbObject$methods( .assert.inferior = function(param1, param2, msg.type = 'error') {
	if (any(param1 > param2)) {
		param1.name <- as.character(sys.call(0))[[2]]
		param2.name <- as.character(sys.call(0))[[3]]
		.self$message(msg.type, paste(param1.name, ' (', param1, ') cannot be greater than ', param2.name, ' (', param2, ').', sep = ''))
		return(FALSE)
	}
	return(TRUE)
})

# Assert equal length {{{1
################################################################

BiodbObject$methods( .assert.equal.length = function(param1, param2, msg.type = 'error') {
	if (length(param1) != length(param2)) {
		param1.name <- as.character(sys.call(0))[[2]]
		param2.name <- as.character(sys.call(0))[[3]]
		.self$message(msg.type, paste(param1.name, ' (length ', length(param1), ') has not the same length as ', param2.name, ' (length ', length(param2), ').', sep = ''))
		return(FALSE)
	}
	return(TRUE)
})

# Assert positive {{{1
################################################################

BiodbObject$methods( .assert.positive = function(param, msg.type = 'error', na.allowed = TRUE, zero = TRUE) {

	param.name <- as.character(sys.call(0))[[2]]
	.self$.assert.not.null(param, msg.type = msg.type, param.name = param.name)
	if ( ! na.allowed)
		.self$.assert.not.na(param, msg.type = msg.type, param.name = param.name)

	if (any(param[ ! is.na(param)] < 0) || ( ! zero && any(param[ ! is.na(param)] == 0))) {
		.self$message(msg.type, paste(param.name, ' (', paste(param, collapse = ", "), ') cannot be negative', if (zero) '' else ' or equal to zero', '.', sep = ''))
		return(FALSE)
	}

	return(TRUE)
})

# Assert length one {{{1
################################################################

BiodbObject$methods( .assert.length.one = function(param, msg.type = 'error') {
	if (length(param) != 1) {
		param.name <- as.character(sys.call(0))[[2]]
		.self$message(msg.type, paste('Length of ', param.name, ' (', length(param), ') must be one.', sep = ''))
		return(FALSE)
	}
	return(TRUE)
})


# Assert in {{{1
################################################################

BiodbObject$methods( .assert.in = function(param, values, msg.type = 'error') {
	if ( ! is.na(param) && ! param %in% values) {
		param.name <- as.character(sys.call(0))[[2]]
		.self$message(msg.type, paste(param.name, ' cannot be set to ', param, '. Allowed values are: ', paste(values, collapse = ', '), '.', sep = ''))
		return(FALSE)
	}
	return(TRUE)
})

# Assert is {{{1
################################################################

BiodbObject$methods( .assert.is = function(param, type, msg.type = 'error') {
	if ( ! is.null(param) && class(param) != type) {
		param.name <- as.character(sys.call(0))[[2]]
		.self$message(msg.type, paste(param.name, ' is not of type ', type, ' but of type ', class(param), '.', sep = ''))
		return(FALSE)
	}
	return(TRUE)
})

# Get biodb {{{1
################################################################

BiodbObject$methods( getBiodb = function() {
	.self$abstract.method()
})

# Message {{{1
################################################################

# Send a message to observers
BiodbObject$methods( message = function(type, msg) {

	# Get biodb instance
	biodb <- NULL
	if (length(.self$.message.enabled) == 1 # length is 0 if called from constructor
		&& .self$.message.enabled) {
		.message.enabled <<- FALSE
		biodb <- .self$getBiodb() 
		.message.enabled <<- TRUE
	}

	# Get class and method information
	class <- class(.self)
	method <- sys.call(length(sys.calls()) - 1)
	method <- sub('^[^$]*\\$([^(]*)(\\(.*)?$', '\\1()', method)[[1]]

	if ( ! is.null(biodb))
		lapply(biodb$getObservers(), function(x) x$message(type = type, msg = msg, class = class, method = method))
	else {
		caller.info <- if (is.na(class)) '' else class
		if (! is.na(method))
			caller.info <- paste(caller.info, method, sep = '::')
		if (nchar(caller.info) > 0) caller.info <- paste('[', caller.info, '] ', sep = '')
		switch(type,
		       ERROR = stop(paste0(caller.info, msg)),
		       WARNING = warning(paste0(caller.info, msg)),
		       cat(caller.info, msg, "\n", file = stderr()))
	}
})
