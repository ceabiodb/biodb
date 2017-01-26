# vi: fdm=marker

# Class declaration {{{1
################################################################

#'The mother abstract class of all connection classes.
#'@export
Biodb <- methods::setRefClass("Biodb", contains = "BiodbObject", fields = list( .factory = "ANY", .observers = "ANY", .useragent = "character", .use.env.var = "logical" ))

# Constructor {{{1
################################################################

Biodb$methods( initialize = function(useragent = NA_character_, use.env.var = TRUE, logger = TRUE, ...) {

	.use.env.var <<- use.env.var

	# Set useragent
	.useragent <<- useragent
	if (is.na(useragent) && .self$.use.env.var) {
		env <- Sys.getenv()
		email <- env[["EMAIL"]]
		if ( ! is.na(email))
			.useragent <<- paste("Biodb user", email, sep = " ; ")
	}

	# Set observers
	.observers <<- list(WarningReporter$new(), ErrorReporter$new())
	if (logger)
		.self$addObservers(BiodbLogger$new())

	# Create factory
	.factory <<- BiodbFactory$new(biodb = .self)

	callSuper(...)
})

# Get biodb {{{1
################################################################

Biodb$methods( getBiodb = function() {
	return(.self)
})

# Get user agent {{{1
################################################################

Biodb$methods( getUserAgent = function() {
	return(.self$.useragent)
})

# Get env var {{{1
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

# Get factory {{{1
################################################################

Biodb$methods( getFactory = function() {
	return(.self$.factory)
})

# Add observers {{{1
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

# Get observers {{{1
################################################################

Biodb$methods( getObservers = function() {
	return(.self$.observers)
})

# Entries to data frame {{{1
################################################################

Biodb$methods( entriesToDataframe = function(entries, only.atomic = TRUE) {
	"Convert a list of entries (BiodbEntry objects) into a data frame."

	# Check classes
	all(vapply(entries, function(x) is(x, 'BiodbEntry'), FUN.VALUE = TRUE)) || .self$message(BIODB.ERROR, "Some objects in the input list ar not a subclass of BiodbEntry.")

	entries.df <- NULL

	# Loop on all entries
	n <- 0
	for (e in entries) {
		n <- n + 1
		.self$message(BIODB.DEBUG, paste("Processing entry", n, "/", length(entries), "..."))
		e.df <- e$getFieldsAsDataFrame(only.atomic = only.atomic)
		entries.df <- plyr::rbind.fill(entries.df, e.df)
	}

	return(entries.df)
})
