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
CFG.DBDWNLD             <- 'dbdwnld'
CFG.MASSBANK.URL        <- 'massbank_url'
CFG.PEAKFOREST.TOKEN    <- 'peakforest_token'
CFG.PEAKFOREST.URL      <- 'peakforest_url'
CFG.USERAGENT           <- 'useragent'

# Value types {{{2
################################################################

.CFG.VALUE.TYPES <- c(
	# Key                   Type
	CFG.CACHEDIR,           'character',
	CFG.CHEMSPIDER.TOKEN,   'character',
	CFG.DBDWNLD,            'logical',
	CFG.MASSBANK.URL,       'character',
	CFG.PEAKFOREST.TOKEN,   'character',
	CFG.PEAKFOREST.URL,     'character',
	CFG.USERAGENT,          'character'
	)
.CFG.VALUE.TYPES <- data.frame(matrix(.CFG.VALUE.TYPES, byrow = TRUE, ncol = 2), stringsAsFactors = FALSE)
colnames(.CFG.VALUE.TYPES) <- c('key', 'type')

.dup.keys <- duplicated(.CFG.VALUE.TYPES[['key']])
if (any(.dup.keys))
	stop(paste("Keys", paste(.CFG.VALUE.TYPES[.dup.keys, 'key'], collpase = ", "),"are duplicated in .CFG.VALUE.TYPES."))

# Constructor {{{1
################################################################

BiodbConfig$methods( initialize = function(biodb = NULL, ...) {

	callSuper(...)

	.biodb <<- biodb
	.values <<- list()
	.env <<- Sys.getenv()

	# Set some default values
	.self$enable(CFG.DBDWNLD)

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

# Get type {{{1
################################################################

BiodbConfig$methods( .getType = function(key) {

	.self$.checkKey(key)

	return(.CFG.VALUE.TYPES[key == .CFG.VALUE.TYPES[['key']], 'type'])
})

# Check key {{{1
################################################################

BiodbConfig$methods( .checkKey = function(key, type = NA_character_) {

	# Check parameters
	if (is.null(key) || is.na(key) || ! is.character(key))
		.self$message(MSG.ERROR, "Key is NULL, NA or not character type.")

	# Test if valid key
	if ( ! key %in% .CFG.VALUE.TYPES[['key']])
		.self$message(MSG.ERROR, paste("Unknown key ", key, ".", sep = ''))

	# Test type
	if ( ! is.na(type) && .self$.getType(key) != type)
		.self$message(MSG.ERROR, paste("Key ", key, " is not of type ", type, " but of type ", key.type, ".", sep = ''))
})

# Defined {{{1
################################################################

BiodbConfig$methods( isDefined = function(key) {

	.self$.checkKey(key)

	return(key %in% names(.self$.values))
})

# Enabled {{{1
################################################################

BiodbConfig$methods( isEnabled = function(key) {

	.self$.checkKey(key, type = 'logical')

	# Defined ?
	if (isDefined(key))
		return(.self$.values[[key]])

	return(FALSE)
})

# Get {{{1
################################################################

BiodbConfig$methods( get = function(key) {

	value <- NULL

	.self$.checkKey(key)

	# Is value defined ?
	if (.self$isDefined(key))
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

	.self$.values[[key]] <- as.vector(value, mode = .self$.getType(key))
})

# Enable {{{1
################################################################

BiodbConfig$methods( enable = function(key) {

	.self$.checkKey(key, type = 'logical')

	.self$.values[[key]] <- TRUE
})

# Disable {{{1
################################################################

BiodbConfig$methods( disable = function(key) {

	.self$.checkKey(key, type = 'logical')

	.self$.values[[key]] <- FALSE
})
