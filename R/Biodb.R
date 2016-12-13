# vi: fdm=marker

# CLASS DECLARATION {{{1
################################################################

Biodb <- methods::setRefClass("Biodb", contains = "BiodbObject", fields = list( .factory = "ANY", .observers = "ANY", .useragent = "character", .use.env.var = "logical" ))

# CONSTRUCTOR {{{1
################################################################

Biodb$methods( initialize = function(useragent = NA_character_, use.env.var = FALSE, ...) {

	.useragent <<- useragent
	.use.env.var <<- use.env.var
	.observers <<- list(WarningReporter$new(), ErrorReporter$new())
	.factory <<- BiodbFactory$new()

	callSuper(...)
})

# GET USER AGENT {{{1
################################################################

Biodb$methods( getUserAgent = function() {
	return(.self$.useragent)
})

# GET ENV VAR {{{1
################################################################

Biodb$methods( getEnvVar = function(name) {

	value <- NA_character_

	if (.self$.use.env.var) {

		# Get all env vars
		env <- Sys.getenv()

		# Make env var name
		env.var <- paste(c('BIODB', toupper(name)), collapse = '_')

		# Look if this env var exists
		if (env.var %in% names(env))
			return(env[[env.var]])
	}

	return(value)
})

# GET FACTORY {{{1
################################################################

Biodb$methods( getFactory = function() {
	return(.self$.factory)
})

# ADD OBSERVERS {{{1
################################################################

Biodb$methods( addObservers = function(obs) {

	# Check types of observers
	if ( ! is.list(obs)) obs <- list(obs)
	is.obs <- vapply(obs, function(o) is(o, "BiodbObserver"), FUN.VALUE = TRUE)
	if (any( ! is.obs))
		stop("Observers must inherit from BiodbObserver class.")

	# Add observers to current list (insert at beginning)
	.observers <<- if (is.null(.self$.observers)) obs else c(obs, .self$.observers)
})

# GET OBSERVERS {{{1
################################################################

Biodb$methods( getObservers = function() {
	return(.self$.observers)
})
