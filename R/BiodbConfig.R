# vi: fdm=marker ts=4 et cc=80

# BiodbConfig {{{1
################################################################################

#' A class for storing configuration values.
#'
#' This class is responsible for storing configuration. You must go through the
#' single instance of this class to create and set and get configuration values.
#' To get the single instance of this class, call the \code{getConfig()} method
#' of class \code{Biodb}.
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
#' if (config$isEnabled('offline'))
#'   print('Mode offline is ON.')
#' else
#'   print('Mode offline is OFF.')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @include BiodbChildObject.R
#' @include BiodbObserver.R
#' @export BiodbConfig
#' @exportClass BiodbConfig
BiodbConfig <- methods::setRefClass("BiodbConfig",
    contains=c('BiodbChildObject', 'BiodbObserver'),

# Fields {{{2
################################################################################
                                    
fields=list(
    .values="list",
    .env="ANY",
    .keys="list"
),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)

    .self$.env <- Sys.getenv()
    .self$.keys <- list()
    .self$.values <- list()
    
    # Register as observer
    .self$getBiodb()$addObservers(.self)
},

# New observer {{{3
################################################################################

newObserver=function(obs) {
    
    # Loop on all keys
    for(key in names(.self$.values))
        .self$getObservers()$cfgKVUpdate(key, .self$.values[[key]])
},

# Get keys {{{3
################################################################################

getKeys=function() {
    "Get the list of available keys."

    return(names(.self$.keys))
},

# Get description {{{3
################################################################################

getDescription=function(key) {
    "Get the description of a key."

    description <- ''

    .self$.checkKey(key)

    # Get default value
    if ('description' %in% names(.self$.keys[[key]]))
        description <- .self$.keys[[key]][['description']]

    return(description)
},

# Get default value {{{3
################################################################################

getDefaultValue=function(key) {
    "Get the default value of a key."

    default <- NULL

    .self$.checkKey(key)

    # Get default value
    if ('default' %in% names(.self$.keys[[key]]))
        default <- .self$.keys[[key]][['default']]

    return(default)
},

# Has key {{{3
################################################################################

hasKey=function(key) {
    "Test if a key exists."

    return(.self$.checkKey(key, fail=FALSE))
},

# Defined {{{3
################################################################################

isDefined=function(key, fail=TRUE) {
    "Test if a key is defined."

    if (.self$.checkKey(key, fail=fail))
        return(key %in% names(.self$.values))
    
    return(FALSE)
},

# Is enabled {{{3
################################################################################

isEnabled=function(key) {
    "Test if a boolean key is set to TRUE."

    .self$.checkKey(key, type='logical')

    value <- FALSE

    # Defined ?
    if (isDefined(key))
        value <- .self$.values[[key]]

    return(value)
},

# Get {{{3
################################################################################

get=function(key) {
    "Get the value of a key."

    .self$.checkKey(key)

    # Is value defined ?
    if (.self$isDefined(key))
        value <- .self$.values[[key]]
    else
        value <- as.vector(NA, mode=.self$.getType(key))

    print('-------------------------------- BiodbConfig::get')
    print(key)
    print(value)
    return(value)
},

# Set {{{3
################################################################################

set=function(key, value) {
    "Set the value of a key."

    .self$.checkKey(key)

    displayed.value <- if (is.character(value)) paste0('"', value, '"')
                       else value
    .self$message('info', paste("Set ", key, ' to ', displayed.value, '.',
                                sep=''))
    v <- as.vector(value, mode=.self$.getType(key))
    .self$.values[[key]] <- v
    .self$debug('Set key ', key, ' to value "', v, '".')
    
    # Notify observers
    lapply(.self$getBiodb()$getObservers(), function(o) o$cfgKVUpdate(key, v))
},

# Enable {{{3
################################################################################

enable=function(key) {
    "Set a boolean key to TRUE."

    .self$.checkKey(key, type='logical')

    .self$message('info', paste("Enable ", key, ".", sep=''))
    .self$.values[[key]] <- TRUE
},

# Disable {{{3
################################################################################

disable=function(key) {
    "Set a boolean key to FALSE."

    .self$.checkKey(key, type='logical')

    .self$message('info', paste("Disable ", key, ".", sep=''))
    .self$.values[[key]] <- FALSE
},

# Show {{{3
################################################################################

show=function() {
    "Print containt of this object in a human readable format."

    cat("Biodb configuration instance.\n")
    cat("  Values:\n")

    # Loop on all keys
    for (key in sort(.self$getKeys()))
        if ( ! .self$.isDeprecated(key))
            cat("    ", key, ": ", .self$get(key), "\n")
},

# List keys {{{3
################################################################################

listKeys=function() {

    # Fields to extract
    field2title <- c(type="Type", default="Default value",
                     description="Description")

    # Build data frame
    df <- data.frame(Key=names(.self$.keys), stringsAsFactors=FALSE)
    for (field in names(field2title))
        df[[field2title[[field]]]] <-
            vapply(.self$.keys,
                   function(k) if ( ! field %in% names(k)
                                   || is.null(k[[field]])) ''
               else as.character(k[[field]]), FUN.VALUE='')

    return(df)
},

# Get associated environment variable {{{3
################################################################################

getAssocEnvVar=function(key) {
    "Returns the environment variable associated with this configuration key."

    # Check key
    .self$.checkKey(key)

    # Build env var
    env.var <- paste(c('BIODB', toupper(gsub('.', '_', key, fixed=TRUE))),
                     collapse='_')

    return(env.var)
},

# Private methods {{{2
################################################################################

# Get SVN binary path {{{3
################################################################################

.getSvnBinaryPath=function() {

    svn.path <- ''

    # Look in system PATH
    svn.path <- Sys.which("svn")[[1]]

    # On Windows, look in common locations
    if (is.null(svn.path) && .Platform$OS.type == "windows") {
        look_in <- c("C:/Program Files/Svn/bin/svn.exe",
                     "C:/Program Files (x86)/Svn/bin/svn.exe",
                     "C:/Program Files/SlikSvn/bin/svn.exe")
        found <- file.exists(look_in)
        if (any(found))
            svn.path <- look_in[found][1]
    }

    return(svn.path)
},

# Load definitions {{{3
################################################################################

.loadDefinitions=function(file) {
},

# Define {{{3
################################################################################

define=function(def) {
    'Define config properties from a structured object, normally loaded from a
    YAML file.'

    .self$debug('Define config keys.')
    # Loop on all keys
    for (key in names(def)) {
        
        v <- def[[key]]
        v$key <- key
        .self$debug('Define config key ', key, '.')
        do.call(.self$.newKey, v)
    }
},

# Get from env {{{3
################################################################################

.getFromEnv=function(key) {

    value <- NULL

    # Look into ENV
    envvar <- paste(c('BIODB', toupper(gsub('.', '_', key, fixed=TRUE))),
                    collapse='_')
    if (envvar %in% names(.self$.env)) {
        value <- .self$.env[[envvar]]
        .self$info2("Found env var ", envvar, ', value "', value,
                    '", defining default value for config key ', key, '.')
    }

    return(value)
},

# New key {{{3
################################################################################

.newKey=function(key, type, default=NULL, description=NA_character_,
                   deprecated=NULL) {

    # Check key
    if (is.null(key) || is.na(key) || ! is.character(key))
        .self$message('error', "Key is NULL, NA or not character type.")

    # Check duplicated key
    if (key %in% names(.self$.keys))
        .self$message('error',
                      paste("Key", key,
                             "has already been defined in configuration."))

    # Overwrite default value by env var, if defined
    env.var.value <- .self$.getFromEnv(key)
    if ( ! is.null(env.var.value))
        default <- env.var.value
    if (is.null(default)) {
        if (key == 'cache.directory' && 'HOME' %in% names(.self$.env))
            default <- file.path(.self$.env[['HOME']], '.biodb.cache')
        if (key == 'useragent' && 'EMAIL' %in% names(.self$.env))
            default <- paste('Biodb user', .self$.env[['EMAIL']], sep=' ; ')
        if (key == 'svn.binary.path')
            default <- .self$.getSvnBinaryPath()
    }
    print('-------------------------------- BiodbConfig::.newKey')
    print(key)
    print(default)

    # Define new key
    .self$.keys[[key]] <- list(type=type, default=default,
                               description=description)

    # Set as deprecated
    if ( ! is.null(deprecated))
        .self$.keys[[key]][['deprecated']] <- deprecated
    
    # Initialize value
    if (is.null(deprecated) && ! is.null(default))
        .self$set(key, default)
},

# Check key {{{3
################################################################################

.checkKey=function(key, type=NA_character_, fail=TRUE,
                     test.deprecated=TRUE) {

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
            .self$message('error', paste0("Unknown key ", key, "."))
        else
            return(FALSE)
    }

    # Fail if deprecated
    if (.self$.isDeprecated(key))
        .self$message('error', paste("Key", key, "is deprecated.",
                                     .self$.keys[[key]][['deprecated']]))

    # Test type
    if ( ! is.null(type) && ! is.na(type)
        && .self$.keys[[key]][['type']] != type) {
        if (fail)
            .self$message('error', paste0("Key ", key, " is not of type ", type,
                                          " but of type ", key.type, "."))
        else
            return(FALSE)
    }

    return(TRUE)
},

# Get type {{{3
################################################################################

.getType=function(key) {

    .self$.checkKey(key)

    return(.self$.keys[[key]][['type']])
},


# Is deprecated {{{3
################################################################################

.isDeprecated=function(key) {
    return('deprecated' %in% names(.self$.keys[[key]]))
}

))
