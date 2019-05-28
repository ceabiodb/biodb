# vi: fdm=marker

# Class declaration {{{1
################################################################

#' A class for storing configuration values.
#'
#' This class is responsible for storing configuration. You must go through the single instance of this class to create and set and get configuration values. To get the single instance of this class, call the \code{getConfig()} method of class \code{Biodb}.
#'
#' @seealso \code{\link{Biodb}}.
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
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @include BiodbChildObject.R
#' @export BiodbConfig
#' @exportClass BiodbConfig
BiodbConfig <- methods::setRefClass("BiodbConfig", contains = "BiodbChildObject", fields = list( .values = "list", .env = "ANY", .keys = "list" ))

# Constructor {{{1
################################################################

BiodbConfig$methods( initialize = function(...) {

	callSuper(...)

	.env <<- Sys.getenv()
	.self$.defineKeys()
	.self$.initValues()
})

# Get keys {{{1
################################################################

BiodbConfig$methods( getKeys = function() {
	":\n\nGet the list of available keys."

	return(names(.self$.keys))
})

# Get description {{{1
################################################################

BiodbConfig$methods( getDescription = function(key) {
	":\n\nGet the description of a key."

	description <- ''

	.self$.checkKey(key)

	# Get default value
	if ('description' %in% names(.self$.keys[[key]]))
		description <- .self$.keys[[key]][['description']]

	return(description)
})

# Get default value {{{1
################################################################

BiodbConfig$methods( getDefaultValue = function(key) {
	":\n\nGet the default value of a key."

	default <- NULL

	.self$.checkKey(key)

	# Get default value
	if ('default' %in% names(.self$.keys[[key]]))
		default <- .self$.keys[[key]][['default']]

	return(default)
})

# Has key {{{1
################################################################

BiodbConfig$methods( hasKey = function(key) {
	":\n\nTest if a key exists."

	return(.self$.checkKey(key, fail = FALSE))
})

# Defined {{{1
################################################################

BiodbConfig$methods( isDefined = function(key, fail = TRUE) {
	":\n\nTest if a key is defined."

	if (.self$.checkKey(key, fail = fail))
		return(key %in% names(.self$.values))
	
	return(FALSE)
})

# Is enabled {{{1
################################################################

BiodbConfig$methods( isEnabled = function(key) {
	":\n\nTest if a boolean key is set to TRUE."

	.self$.checkKey(key, type = 'logical')

	value <- FALSE

	# Defined ?
	if (isDefined(key))
		value <- .self$.values[[key]]

	return(value)
})

# Get {{{1
################################################################

BiodbConfig$methods( get = function(key) {
	":\n\nGet the value of a key."

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
	":\n\nSet the value of a key."

	.self$.checkKey(key)

	displayed.value <- if (is.character(value)) paste0('"', value, '"') else value
	.self$message('info', paste("Set ", key, ' to ', displayed.value, '.', sep = ''))
	.self$.values[[key]] <- as.vector(value, mode = .self$.getType(key))
})

# Enable {{{1
################################################################

BiodbConfig$methods( enable = function(key) {
	":\n\nSet a boolean key to TRUE."

	.self$.checkKey(key, type = 'logical')

	.self$message('info', paste("Enable ", key, ".", sep = ''))
	.self$.values[[key]] <- TRUE
})

# Disable {{{1
################################################################

BiodbConfig$methods( disable = function(key) {
	":\n\nSet a boolean key to FALSE."

	.self$.checkKey(key, type = 'logical')

	.self$message('info', paste("Disable ", key, ".", sep = ''))
	.self$.values[[key]] <- FALSE
})

# Show {{{1
################################################################

BiodbConfig$methods( show = function() {
	":\n\nPrint containt of this object in a human readable format."

	cat("Biodb configuration instance.\n")
	cat("  Values:\n")

	# Loop on all keys
	for (key in sort(.self$getKeys()))
		if ( ! .self$.isDeprecated(key))
			cat("    ", key, ": ", .self$get(key), "\n")
})

# List keys {{{1
################################################################

BiodbConfig$methods( listKeys = function() {

	# Fields to extract
	field2title <- c(type = "Type", default = "Default value", description = "Description")

	# Build data frame
	df <- data.frame(Key = names(.self$.keys), stringsAsFactors = FALSE)
	for (field in names(field2title))
		df[[field2title[[field]]]] <- vapply(.self$.keys, function(k) if ( ! field %in% names(k) || is.null(k[[field]])) '' else as.character(k[[field]]), FUN.VALUE = '')

	return(df)
})

# Get associated environment variable {{{1
################################################################

BiodbConfig$methods( getAssocEnvVar = function(key) {
	":\n\nReturns the environment variable associated with this configuration key."

	# Check key
	.self$.checkKey(key)

	# Build env var
	env.var <- paste(c('BIODB', toupper(gsub('.', '_', key, fixed = TRUE))), collapse = '_')

	return(env.var)
})

# PRIVATE METHODS {{{1
################################################################

# Get SVN binary path {{{2
################################################################

BiodbConfig$methods( .get.svn.binary.path = function() {

	svn.path <- ''

	# Look in system PATH
	svn.path <- Sys.which("svn")[[1]]

	# On Windows, look in common locations
	if (is.null(svn.path) && .Platform$OS.type == "windows") {
		look_in <- c("C:/Program Files/Svn/bin/svn.exe", "C:/Program Files (x86)/Svn/bin/svn.exe", "C:/Program Files/SlikSvn/bin/svn.exe")
		found <- file.exists(look_in)
		if (any(found))
			svn.path <- look_in[found][1]
	}

	return(svn.path)
})

# Define keys {{{2
################################################################

BiodbConfig$methods( .defineKeys = function() {

	.keys <<- list()

	# Define default values
	cachedir.default  <- if ('HOME' %in% names(.self$.env)) file.path(.self$.env[['HOME']], '.biodb.cache') else NULL
	useragent.default <- if ('EMAIL' %in% names(.self$.env)) paste('Biodb user', .self$.env[['EMAIL']], sep = ' ; ') else NULL

	# Define keys
	.self$.newKey('allow.huge.downloads',       type = 'logical',   description = "Download of huge files like whole database data is allowed.", default = TRUE)
	.self$.newKey('cache.all.requests',         type = 'logical',   description = "All requests are cached. If disabled, only requests of entries by accession number are cached.", default = TRUE)
	.self$.newKey('cache.directory',            type = 'character', description = "The directory in which cache files are stored.", default = cachedir.default)
	.self$.newKey('cache.read.only',            type = 'logical',   description = "The cache system is not writable. This is mainly used for test purposes.", default = FALSE)
	.self$.newKey('cache.subfolders',           type = 'logical',   description = "Use subfolders shortterm and longterm in cache system, in order to divide the downloaded files. If a whole database is downloaded, the file(s) will be put in the longterm subfolders.", default = TRUE)
	.self$.newKey('cache.system',               type = 'logical',   description = "Cache system is ON.", default = TRUE)
	.self$.newKey('dwnld.chunk.size',           type = 'integer',   description = 'The number of entries to download before saving to cache. By default, saving to cache is only down once all requested entries have been downloaded.', default = NA_integer_)
	.self$.newKey('factory.cache',              type = 'logical',   description = "Factory cache system is ON. The factory cache system stores entry instances already created. When the factory cache is enabled and an entry already created is requested, the factory returns that same instance. This is possible, because biodb uses the RefClass OOP system, which uses references instead of object copy. Thus, if you modify an entry and ask the factory for the same entry, it will be the same exact object that will be given to you and it will include your modifications. On the contrary, if the factory cache is OFF, a new entry instance will be returned to you each time. The factory cache enables to speed up entry retrieval when the same entries are requested several times, avoiding the parsing process.", default = FALSE, deprecated = "The cache is now always enabled. Thus all entries returned are referenced inside the cache and will be returned when needed.")
	.self$.newKey('compute.fields',             type = 'logical',   description = "If the field of an entry is accessed but has no value, then biodb will try to compute one. This is done by following rules that tell biodb in which database to look for this field's value.", default = TRUE)
	.self$.newKey('force.locale',               type = 'logical',   description = "Forcing current locale is allowed.", default = TRUE)
	.self$.newKey('longterm.cache.subfolder',   type = 'character', description = "The name of the long term cache subfolder.",  default = 'longterm')
	.self$.newKey('multival.field.sep',         type = 'character', description = "The character used as separator when concatenating multiple values of a field.",  default = ';')
	.self$.newKey('offline',                    type = 'logical',   description = "All network accesses are blocked. This is mainly used for test purposes.", default = FALSE)
	.self$.newKey('shortterm.cache.subfolder',  type = 'character', description = "The name of the short term cache subfolder.", default = 'shortterm')
	.self$.newKey('svn.binary.path',            type = 'character', description = "The path to the SubVersion binary (svn).", default = .self$.get.svn.binary.path())
	.self$.newKey('useragent',                  type = 'character', description = "The user agent description string. This string is compulsory when connection to remote databases.", default = useragent.default)
	.self$.newKey('chemspider.token',           type = 'character', description = 'The token to access ChemSpider database.')
	.self$.newKey('peakforest.mass.token',      type = 'character', description = 'The token to access PeakForest Mass database.')
	.self$.newKey('peakforest.compound.token',  type = 'character', description = 'The token to access PeakForest Compound database.')
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

BiodbConfig$methods( .newKey = function(key, type, default = NULL, description = NA_character_, deprecated = NULL) {

	# Check key
	if (is.null(key) || is.na(key) || ! is.character(key))
		.self$message('error', "Key is NULL, NA or not character type.")

	# Check duplicated key
	if (key %in% names(.self$.keys))
		.self$message('error', paste("Key ", key, " has already been defined in configuration.", sep = ''))

	# Overwrite default value by env var, if defined
	env.var.value <- .self$.getFromEnv(key)
	if ( ! is.null(env.var.value))
		default <- env.var.value

	# Define new key
	.self$.keys[[key]] <- list(type = type, default = default, description = description)

	if ( ! is.null(deprecated))
		.self$.keys[[key]][['deprecated']] <- deprecated
})

# Initialize values {{{2
################################################################

BiodbConfig$methods( .initValues = function() {

	.values <<- list()

	# Loop on all keys
	for (key in .self$getKeys()) {
		
		if (.self$.isDeprecated(key))
			next

		default <- .self$getDefaultValue(key)

		# Set default value if not null
		if ( ! is.null(default))
			.self$set(key, default)
	}
})

# Check key {{{2
################################################################

BiodbConfig$methods( .checkKey = function(key, type = NA_character_, fail = TRUE, test.deprecated = TRUE) {

	# Check key
	if (is.null(key) || is.na(key) || ! is.character(key)) {
		if (fail)
			.self$message('error', "Key is NULL, NA or not character type.")
		else
			return(FALSE)
	}

	# Fail if invalid key
	if ( ! key %in% names(.self$.keys)) {
		if (fail)
			.self$message('error', paste("Unknown key ", key, ".", sep = ''))
		else
			return(FALSE)
	}

	# Fail if deprecated
	if (.self$.isDeprecated(key))
		.self$message('error', paste("Key ", key, " is deprecated. ", .self$.keys[[key]][['deprecated']], sep = ''))

	# Test type
	if ( ! is.null(type) && ! is.na(type) && .self$.keys[[key]][['type']] != type) {
		if (fail)
			.self$message('error', paste("Key ", key, " is not of type ", type, " but of type ", key.type, ".", sep = ''))
		else
			return(FALSE)
	}

	return(TRUE)
})

# Get type {{{2
################################################################

BiodbConfig$methods( .getType = function(key) {

	.self$.checkKey(key)

	return(.self$.keys[[key]][['type']])
})


# Is deprecated {{{2
################################################################

BiodbConfig$methods( .isDeprecated = function(key) {
	return('deprecated' %in% names(.self$.keys[[key]]))
})
