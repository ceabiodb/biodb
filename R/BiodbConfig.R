# vi: fdm=marker

# Class declaration {{{1
################################################################

#' A class for storing configuration values.
#'
#' This class is responsible for storing configuration. You must go through the single instance of this class to create and set and get configuration values. To get the single instance of this class, call the \code{getConfig()} method of class \code{Biodb}.
#'
#' @seealso \code{\link\{Biodb}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get the config instance:
#' config <- mybiodb$getConfig()
#'
#' # Print all available keys
#' print(config$getKeys())
#'
#' # Get a configuration value:
#' value <- config$get('cache.directory')
#' 
#' # Set a configuration value:
#' config$set('cache.directory', '~/my.biodb.cache')
#'
#' # For boolean values, you can use boolean methods:
#' print(config$get('offline'))
#' config$enable('offline')    # set to TRUE
#' config$disable('offline')   # set to FALSE
#' print(if (config$isEnabled('offline')) 'Mode offline is ON.' else 'Mode offline is OFF.')
#'
#' @import methods
#' @include ChildObject.R
#' @export BiodbConfig
#' @exportClass BiodbConfig
BiodbConfig <- methods::setRefClass("BiodbConfig", contains = "ChildObject", fields = list( .values = "list", .env = "ANY", .value.info = "list" ))

# Constructor {{{1
################################################################

BiodbConfig$methods( initialize = function(...) {

	callSuper(...)

	.env <<- Sys.getenv()
	.self$.initValueInfo()
	.self$.initValues()
})

# Get keys {{{1
################################################################

BiodbConfig$methods( getKeys = function() {
	":\n\nGet the list of available keys."

	return(names(.self$.value.info))
})

# Get description {{{1
################################################################

BiodbConfig$methods( getDescription = function(key) {
	":\n\nGet the description of a key."

	description <- ''

	.self$.checkKey(key)

	# Get default value
	if ('description' %in% names(.self$.value.info[[key]]))
		description <- .self$.value.info[[key]][['description']]

	return(description)
})

# Get default value {{{1
################################################################

BiodbConfig$methods( getDefaultValue = function(key) {
	":\n\nGet the default value of a key."

	default <- NULL

	.self$.checkKey(key)

	# Get default value
	if ('default' %in% names(.self$.value.info[[key]]))
		default <- .self$.value.info[[key]][['default']]

	return(default)
})

# Defined {{{1
################################################################

BiodbConfig$methods( isDefined = function(key) {
	":\n\nTest if a key is defined."

	.self$.checkKey(key)

	return(key %in% names(.self$.values))
})

# Is enabled {{{1
################################################################

BiodbConfig$methods( isEnabled = function(key) {
	":\n\nTest if a boolean key is set to TRUE."

	.self$.checkKey(key, type = 'logical')

	# Defined ?
	if (isDefined(key))
		return(.self$.values[[key]])

	return(FALSE)
})

# Get {{{1
################################################################

BiodbConfig$methods( get = function(key) {
	":\n\nGet a the value of a key."

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
	":\n\nSet a the value of a key."

	.self$.checkKey(key)

	.self$message(MSG.INFO, paste("Set ", key, " to ", value, ".", sep = ''))
	.self$.values[[key]] <- as.vector(value, mode = .self$.getType(key))
})

# Enable {{{1
################################################################

BiodbConfig$methods( enable = function(key) {
	":\n\nSet a boolean key to TRUE."

	.self$.checkKey(key, type = 'logical')

	.self$message(MSG.INFO, paste("Enable ", key, ".", sep = ''))
	.self$.values[[key]] <- TRUE
})

# Disable {{{1
################################################################

BiodbConfig$methods( disable = function(key) {
	":\n\nSet a boolean key to FALSE."

	.self$.checkKey(key, type = 'logical')

	.self$message(MSG.INFO, paste("Disable ", key, ".", sep = ''))
	.self$.values[[key]] <- FALSE
})

# PRIVATE METHODS {{{1
################################################################

# Get SVN binary path {{{2
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

# Initialize value information {{{2
################################################################

BiodbConfig$methods( .initValueInfo = function() {

	.value.info <<- list()


	# Define default values
	cachedir.default  <- if ('HOME' %in% names(.self$.env)) file.path(.self$.env[['HOME']], '.biodb.cache') else NULL
	useragent.default <- if ('EMAIL' %in% names(.self$.env)) paste('Biodb user', .self$.env[['EMAIL']], sep = ' ; ') else NULL

	# Define keys
	.self$.newKey('allow.huge.downloads',   type = 'logical',   description = "Download of huge files like whole database data is allowed.", default = TRUE)
	.self$.newKey('cache.directory',        type = 'character', description = "The directory in which cache files are stored.", default = cachedir.default)
	.self$.newKey('cache.system',           type = 'logical',   description = "Cache system is ON.", default = TRUE)
	.self$.newKey('cache.all.requests',     type = 'logical',   description = "All requests are cached. If disabled, only requests of entries by accession number are cached.", default = TRUE)
	.self$.newKey('cache.read.only',        type = 'logical',   description = "The cache system is not writable. This is mainly used for test purposes.", default = FALSE)
	.self$.newKey('compute.fields',         type = 'logical',   description = "If the field of an entry is accessed but has no value, then biodb will try to compute one. This is done by following rules that tell biodb in which database to look for this field's value.", default = TRUE)
	.self$.newKey('offline',                type = 'logical',   description = "All network accesses are blocked. This is mainly used for test purposes.", default = FALSE)
	.self$.newKey('useragent',              type = 'character', description = "The user agent description string. This string is compulsory when connection to remote databases.", default = useragent.default)
	.self$.newKey('cache.subfolders',       type = 'logical',   description = "Use subfolders shortterm and longterm in cache system, in order to divide the downloaded files. If a whole database is downloaded, the file(s) will be put in the longterm subfolders.", default = TRUE)
	.self$.newKey('shortterm.cache.subfolder',  type = 'character',   description = "The name of the short term cache subfolder.", default = 'shortterm')
	.self$.newKey('longterm.cache.subfolder',   type = 'character',   description = "The name of the long term cache subfolder.",  default = 'longterm')
	.self$.newKey('svn.binary.path',        type = 'character', description = "The path to the SubVersion binary (svn).", default = .self$.get.svn.binary.path())
	.self$.newKey('force.c.locale',         type = 'logical',   description = "Forcing current locale to 'C' is allowed.", default = TRUE)

	# TODO Move into DbInfo
	.self$.newKey('chemspider.token',       type = 'character')
})

# Get from env {{{2
################################################################

BiodbConfig$methods( .getFromEnv = function(key) {

	value <- NULL

	# Look into ENV
	envvar <- paste(c('BIODB', toupper(gsub('.', '_', key, fixed = TRUE))), collapse = '_')
	if (envvar %in% names(.self$.env))
		value <- .self$.env[[envvar]]

	return(value)
})

# New key {{{2
################################################################

BiodbConfig$methods( .newKey = function(key, type, default = NULL, description = NA_character_) {

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
	.self$.value.info[[key]] <- list(type = type, default = default, description = description)
})

# Initialize values {{{2
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

# Check key {{{2
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

# Get type {{{2
################################################################

BiodbConfig$methods( .getType = function(key) {

	.self$.checkKey(key)

	return(.self$.value.info[[key]][['type']])
})

