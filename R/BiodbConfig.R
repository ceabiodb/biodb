# vi: fdm=marker

# Class declaration {{{1
################################################################

#' Class for storing configuration values.
#'
#' @include ChildObject.R
#' @export
BiodbConfig <- methods::setRefClass("BiodbConfig", contains = "ChildObject", fields = list( .values = "list", .env = "ANY", .value.info = "list" ))

# Constants {{{1
################################################################

# Keys
CFG.ALLOW.HUGE.DOWNLOADS    <- 'allow.huge.downloads'
CFG.CACHE.DIRECTORY         <- 'cache.directory'
CFG.CACHE.SYSTEM            <- 'cache.system'
CFG.CACHE.READ.ONLY         <- 'cache.read.only'
CFG.CHEMSPIDER.TOKEN        <- 'chemspider_token'
CFG.COMPUTE.FIELDS          <- 'compute.fields'
CFG.OFFLINE                 <- 'offline'
CFG.PEAKFOREST.TOKEN        <- 'peakforest_token'
CFG.PEAKFOREST.URL          <- 'peakforest_url'
CFG.USERAGENT               <- 'useragent'
CFG.USE.CACHE.SUBFOLDERS    <- 'cache.subfolders'
CFG.SVN.BINARY.PATH         <- 'svn.binary.path'

# Database URLs
PEAKFOREST.WS.URL         <- 'https://rest.peakforest.org/'
PEAKFOREST.WS.ALPHA.URL   <- 'https://peakforest-alpha.inra.fr/rest/'

# Constructor {{{1
################################################################

BiodbConfig$methods( initialize = function(...) {

	callSuper(...)

	.env <<- Sys.getenv()
	.self$.initValueInfo()
	.self$.initValues()
})

# Get from env {{{1
################################################################

BiodbConfig$methods( .getFromEnv = function(key) {

	value <- NULL

	# Look into ENV
	envvar <- paste(c('BIODB', toupper(key)), collapse = '_')
	if (envvar %in% names(.self$.env))
		value <- .self$.env[[envvar]]

	return(value)
})

# Initialize value information {{{1
################################################################

BiodbConfig$methods( .initValueInfo = function() {

	.value.info <<- list()


	# Define default values
	cachedir.default  <- if ('HOME' %in% names(.self$.env)) file.path(.self$.env[['HOME']], '.biodb.cache') else NULL
	useragent.default <- if ('EMAIL' %in% names(.self$.env)) paste('Biodb user', .self$.env[['EMAIL']], sep = ' ; ') else NULL

	# Define keys
	.self$.newKey(CFG.ALLOW.HUGE.DOWNLOADS,     type = 'logical',   default = TRUE)
	.self$.newKey(CFG.CACHE.DIRECTORY,          type = 'character', default = cachedir.default)
	.self$.newKey(CFG.CACHE.SYSTEM,             type = 'logical',   default = TRUE)
	.self$.newKey(CFG.CACHE.READ.ONLY,          type = 'logical',   default = FALSE)
	.self$.newKey(CFG.CHEMSPIDER.TOKEN,         type = 'character')
	.self$.newKey(CFG.COMPUTE.FIELDS,           type = 'logical',   default = TRUE)
	.self$.newKey(CFG.OFFLINE,                  type = 'logical',   default = FALSE)
	.self$.newKey(CFG.PEAKFOREST.TOKEN,         type = 'character')
	.self$.newKey(CFG.PEAKFOREST.URL,           type = 'character', default = PEAKFOREST.WS.ALPHA.URL)
	.self$.newKey(CFG.USERAGENT,                type = 'character', default = useragent.default)
	.self$.newKey(CFG.USE.CACHE.SUBFOLDERS,     type = 'logical',   default = TRUE)
	.self$.newKey(CFG.SVN.BINARY.PATH,          type = 'character', default = .self$.get.svn.binary.path())
})

# New key {{{1
################################################################

BiodbConfig$methods( .newKey = function(key, type, default = NULL) {

	# Check key
	if (is.null(key) || is.na(key) || ! is.character(key))
		.self$message(MSG.ERROR, "Key is NULL, NA or not character type.")

	# Check duplicated key
	if (key %in% names(.self$.value.info))
		.self$message(MSG.ERROR, paste("Key ", key, " has already been defined in configuration.", sep = ''))

	# Overwrite default value by env var, if defined
	env.var.value <- .self$.getFromEnv(key)
	if ( ! is.null(env.var.value))
		default <- env.var.value

	# Define new key
	.self$.value.info[[key]] <- list(type = type, default = default)
})

# Get keys {{{1
################################################################

BiodbConfig$methods( getKeys = function() {
	return(names(.self$.value.info))
})

# Get default value {{{1
################################################################

BiodbConfig$methods( getDefaultValue = function(key) {

	default <- NULL

	.self$.checkKey(key)

	# Get default value
	if ('default' %in% names(.self$.value.info[[key]]))
		default <- .self$.value.info[[key]][['default']]

	return(default)
})

# Initialize values {{{1
################################################################

BiodbConfig$methods( .initValues = function() {

	.values <<- list()

	# Loop on all keys
	for (key in .self$getKeys()) {
		default <- .self$getDefaultValue(key)

		# Set default value if not null
		if ( ! is.null(default))
			.self$set(key, default)
	}
})


# Get type {{{1
################################################################

BiodbConfig$methods( .getType = function(key) {

	.self$.checkKey(key)

	return(.self$.value.info[[key]][['type']])
})

# Check key {{{1
################################################################

BiodbConfig$methods( .checkKey = function(key, type = NA_character_) {

	# Check key
	if (is.null(key) || is.na(key) || ! is.character(key))
		.self$message(MSG.ERROR, "Key is NULL, NA or not character type.")

	# Test if valid key
	if ( ! key %in% names(.self$.value.info))
		.self$message(MSG.ERROR, paste("Unknown key ", key, ".", sep = ''))

	# Test type
	if ( ! is.null(type) && ! is.na(type) && .self$.value.info[[key]][['type']] != type)
		.self$message(MSG.ERROR, paste("Key ", key, " is not of type ", type, " but of type ", key.type, ".", sep = ''))
})

# Defined {{{1
################################################################

BiodbConfig$methods( isDefined = function(key) {

	.self$.checkKey(key)

	return(key %in% names(.self$.values))
})

# Is enabled {{{1
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

	.self$.checkKey(key)

	# Is value defined ?
	if (.self$isDefined(key))
		value <- .self$.values[[key]]
	else
		value <- as.vector(NA, mode = .self$.getType(key))

	return(value)
})

# Set {{{1
################################################################

BiodbConfig$methods( set = function(key, value) {

	.self$.checkKey(key)

	.self$message(MSG.INFO, paste("Set ", key, " to ", value, ".", sep = ''))
	.self$.values[[key]] <- as.vector(value, mode = .self$.getType(key))
})

# Enable {{{1
################################################################

BiodbConfig$methods( enable = function(key) {

	.self$.checkKey(key, type = 'logical')

	.self$message(MSG.INFO, paste("Enable ", key, ".", sep = ''))
	.self$.values[[key]] <- TRUE
})

# Disable {{{1
################################################################

BiodbConfig$methods( disable = function(key) {

	.self$.checkKey(key, type = 'logical')

	.self$message(MSG.INFO, paste("Disable ", key, ".", sep = ''))
	.self$.values[[key]] <- FALSE
})

# Get SVN binary path {{{1
################################################################

BiodbConfig$methods( .get.svn.binary.path = function() {

	# Look in system PATH
	svn_path <- Sys.which("svn")[[1]]
	if (svn_path == '')
		svn_path <- NULL

	# On Windows, look in common locations
	if (is.null(svn_path) && .Platform$OS.type == "windows") {
		look_in <- c("C:/Program Files/Svn/bin/svn.exe", "C:/Program Files (x86)/Svn/bin/svn.exe")
		found <- file.exists(look_in)
		if (any(found))
			svn_path <- look_in[found][1]
	}

	# Not found
	if (is.null(svn_path))
		.self$message(MSG.ERROR, "SVN does not seem to be installed on your system.")

	return(svn_path)
})
