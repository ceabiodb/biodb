# vi: fdm=marker

# Class declaration {{{1
################################################################

#'Class for storing configuration values.
#'@export
BiodbConfig <- methods::setRefClass("BiodbConfig", contains = "BiodbObject", fields = list( .biodb = "ANY", .values = "list", .env = "ANY" ))

# Constants {{{1
################################################################

CFG.CACHEDIR            <- 'cachedir'
CFG.CHEMSPIDER.TOKEN    <- 'chemspider_token'
CFG.MASSBANK.URL        <- 'massbank_url'
CFG.PEAKFOREST.TOKEN    <- 'peakforest_token'
CFG.PEAKFOREST.URL      <- 'peakforest_url'
CFG.USERAGENT           <- 'useragent'

# Value types {{{1
################################################################

.CFG.VALUE.TYPES <- c(
	# Key                   Type
	CFG.CACHEDIR,           'character',
	CFG.CHEMSPIDER.TOKEN,   'character',
	CFG.MASSBANK.URL,       'character',
	CFG.PEAKFOREST.TOKEN,   'character',
	CFG.PEAKFOREST.URL,     'character',
	CFG.USERAGENT,          'character'
	)
.CFG.VALUE.TYPES <- data.frame(matrix(.CFG.VALUE.TYPES, byrow = TRUE, ncol = 2), stringsAsFactors = FALSE)
colnames(.CFG.VALUE.TYPES) <- c('key', 'type')

# Constructor {{{1
################################################################

BiodbConfig$methods( initialize = function(biodb = NULL, ...) {

	callSuper(...)

	.biodb <<- biodb
	.values <<- list()
	.env <<- Sys.getenv()

	# Set useragent default value
	if (is.null(.self$get(CFG.USERAGENT))) {
		email <- .self$.env[["EMAIL"]]
		if ( ! is.na(email))
			.self$set(CFG.USERAGENT, paste("Biodb user", email, sep = " ; "))
	}

	# Set cache directory default value
	if (is.null(.self$get(CFG.CACHEDIR))) {
		home <- .self$.env[["HOME"]]
		if ( ! is.na(home))
			.self$set(CFG.CACHEDIR, file.path(home, ".biodb.cache"))
	}
})

# Get biodb {{{1
################################################################

BiodbConfig$methods( getBiodb = function() {
	return(.self$.biodb)
})

# Check key {{{1
################################################################

BiodbConfig$methods( .checkKey = function(key) {

	# Check parameters
	if (is.null(key) || is.na(key) || ! is.character(key))
		.self$message(MSG.ERROR, "Key is NULL, NA or not character type.")
})

# Get {{{1
################################################################

BiodbConfig$methods( get = function(key) {

	value <- NULL

	.self$.checkKey(key)

	# Is value defined ?
	if (key %in% names(.self$.values))
		value <- .self$.values[[key]]

	# Look into ENV
	else {
		envvar <- paste(c('BIODB', toupper(key)), collapse = '_')
		if (envvar %in% names(.self$.env))
			value <- .self$.env[[envvar]]
	}

	return(value)
})

# Set {{{1
################################################################

BiodbConfig$methods( set = function(key, value) {

	.self$.checkKey(key)

	.self$.values[[key]] <- value
})
