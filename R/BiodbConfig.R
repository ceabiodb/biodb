#' A class for storing configuration values.
#'
#' This class is responsible for storing configuration. You must go through the
#' single instance of this class to create and set and get configuration values.
#' To get the single instance of this class, call the \code{getConfig()} method
#' of class \code{BiodbMain}.
#'
#' @seealso \code{\link{BiodbMain}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Get the config instance:
#' config <- mybiodb$getConfig()
#'
#' # Print all available keys
#' config$getKeys()
#'
#' # Get a configuration value:
#' value <- config$get('cache.directory')
#'
#' # Set a configuration value:
#' config$set('dwnld.timeout', 600)
#'
#' # For boolean values, you can use boolean methods:
#' config$get('offline')
#' config$enable('offline')    # set to TRUE
#' config$disable('offline')   # set to FALSE
#' config$isEnabled('offline')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @import rappdirs
#' @include BiodbObserver.R
#' @export BiodbConfig
#' @exportClass BiodbConfig
BiodbConfig <- methods::setRefClass("BiodbConfig",
    contains='BiodbObserver',
    fields=list(
        .values="list",
        .env="ANY",
        .keys="list",
        .parent="ANY"
    ),

methods=list(

initialize=function(parent) {

    .self$.parent <- parent
    .self$.env <- Sys.getenv()
    .self$.keys <- list()
    .self$.values <- list()

    # Register as observer
    .self$.parent$addObservers(.self)
},

getKeys=function(deprecated=FALSE) {
    ":\n\nGet the list of available keys.
    \ndeprecated: If set to TRUE returns also the deprecated keys.
    \nReturned value: A character vector containing the config key names.
    "

    keys <- Filter(function(k) { ! .self$.isDeprecated(k) }, names(.self$.keys))

    return(keys)
},

getTitle=function(key) {
    ":\n\nGet the title of a key.
    \nkey: The name of a configuration key.
    \nReturned value: The title of the key as a character value.
    "

    title <- ''

    .self$.checkKey(key)

    if ('title' %in% names(.self$.keys[[key]]) &&
        length(.self$.keys[[key]][['title']]) > 0)
        title <- .self$.keys[[key]][['title']]

    return(title)
},

getDescription=function(key) {
    ":\n\nGet the description of a key.
    \nkey: The name of a configuration key.
    \nReturned value: The description of the key as a character value.
    "

    description <- ''

    .self$.checkKey(key)

    if ('description' %in% names(.self$.keys[[key]]))
        description <- .self$.keys[[key]][['description']]

    return(description)
},

getDefaultValue=function(key, as.chr=FALSE) {
    ":\n\nGet the default value of a key.
    \nkey: The name of a configuration key.
    \nas.chr: If set to TRUE, returns the value as character.
    \nReturned value: The default value for that key.
    "

    default <- NULL

    .self$.checkKey(key)

    # Get default value
    if ('default' %in% names(.self$.keys[[key]]))
        default <- .self$.keys[[key]][['default']]

    if (as.chr)
        default <- if (is.null(default)) NA_character_ else
            as.character(default)

    return(default)
},

hasKey=function(key) {
    ":\n\nTest if a key exists.
    \nkey: The name of a configuration key.
    \nReturned value: TRUE if a key with this name exists, FALSE otherwise.
    "

    return(.self$.checkKey(key, fail=FALSE))
},

isDefined=function(key, fail=TRUE) {
    ":\n\nTest if a key is defined (i.e.: if a value exists for this key).
    \nkey: The name of a configuration key.
    \nfail: If set to TRUE and the configuration key does not exist, then an
    error will be raised.
    \nReturned value: TRUE if the key has a value, FALSE otherwise.
    "

    if (.self$.checkKey(key, fail=fail))
        return(key %in% names(.self$.values))

    return(FALSE)
},

isEnabled=function(key) {
    ":\n\nTest if a boolean key is set to TRUE. This method will raise an error
    if the key is not a boolean key.
    \nkey: The name of a configuration key.
    \nReturned value: TRUE if the boolean key has a value set to TRUE, FALSE
    otherwise.
    "

    .self$.checkKey(key, type='logical')

    value <- FALSE

    # Defined ?
    if (isDefined(key))
        value <- .self$.values[[key]]

    return(value)
},

get=function(key) {
    ":\n\nGet the value of a key.
    \nkey: The name of a configuration key.
    \nReturned value: The value associated with the key.
    "

    .self$.checkKey(key)

    # Is value defined ?
    if (.self$isDefined(key))
        value <- .self$.values[[key]]
    else
        value <- as.vector(NA, mode=.self$.getType(key))

    return(value)
},

set=function(key, value) {
    ":\n\nSet the value of a key.
    \nkey: The name of a configuration key.
    \nvalue: A value to associate with the key.
    \nReturned value: None.
    "

    .self$.checkKey(key)

    v <- as.vector(value, mode=.self$.getType(key))
    .self$.values[[key]] <- v
    displayed.value <- if (is.character(value)) paste0('"', value, '"')
        else value
    logDebug("Set key %s to %s.", key, displayed.value)

    # Notify observers
    notifyObservers(.self$.parent$getObservers(), 'cfgKVUpdate', list(k=key, v=v))

    invisible(NULL)
},

reset=function(key=NULL) {
    ":\n\nReset the value of a key.
    \nkey: The name of a configuration key. If NULL, all keys will be reset.
    \nReturned value: None.
    "

    # Set keys to reset
    if (is.null(key))
        keys <- names(.self$.keys)
    else {
        .self$.checkKey(key)
        keys <- key
    }

    # Loop on all keys
    for (k in keys)
        .self$set(k, .self$.keys[[key]]$default)
},

enable=function(key) {
    ":\n\nSet a boolean key to TRUE.
    \nkey: The name of a configuration key.
    \nReturned value: None.
    "

    .self$.checkKey(key, type='logical')

    logInfo("Enable %s.", key)
    .self$.values[[key]] <- TRUE
},

disable=function(key) {
    ":\n\nSet a boolean key to FALSE.
    \nkey: The name of a configuration key.
    \nReturned value: None.
    "

    .self$.checkKey(key, type='logical')

    logInfo("Disable %s.", key)
    .self$.values[[key]] <- FALSE
},

show=function() {
    ":\n\nPrint list of configuration keys and their values.
    \nReturned value: None.
    "

    cat("Biodb configuration instance.\n")

    # Loop on all keys
    keys <- sort(.self$getKeys())
    if (length(keys) > 0) {
        cat("  Values:\n")
        for (key in keys)
            if ( ! .self$.isDeprecated(key))
                cat("    ", key, ": ", .self$get(key), "\n")
    }
},

listKeys=function() {
    ":\n\nGet the full list of keys as a data frame.
    \nReturned value: A data frame containing keys, titles, types, and default
    values.
    "

    keys <- .self$getKeys()
    x <- data.frame(key=keys, stringsAsFactors=FALSE)
    x$title <- vapply(keys, function(k) .self$getTitle(k), FUN.VALUE='')
    x$type <- vapply(keys, function(k) .self$.getType(k), FUN.VALUE='')
    x$default <- vapply(keys,
                        function(k) .self$getDefaultValue(k, as.chr=TRUE),
                        FUN.VALUE='')

    return(x)
},

getAssocEnvVar=function(key) {
    ":\n\nReturns the environment variable associated with this configuration
    key.
    \nkey: The name of a configuration key.
    \nReturned value: None.
    "

    # Check key
    .self$.checkKey(key)

    # Build env var
    env.var <- paste(c('BIODB', toupper(gsub('.', '_', key, fixed=TRUE))),
        collapse='_')

    return(env.var)
},

define=function(def) {
    "\n\nDefines config properties from a structured object, normally loaded
    from a YAML file.
    \ndef: The list of key definitions.
    \nReturned value: None.
    "

    # Get key names
    keys <- names(def)

    # Loop on all keys
    for (key in keys) {

        v <- def[[key]]
        v$key <- key
        logDebug('Define config key %s.', key)
        do.call(.self$.newKey, v)
    }
},

newObserver=function(obs) {

    # Loop on all keys
    for(key in names(.self$.values))
        notifyObservers(.self$.parent$getObservers(), 'cfgKVUpdate', list(k=key, v=.self$.values[[key]]))
},

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

.getFromEnv=function(key) {

    value <- NULL

    # Look into ENV
    envvar <- paste(c('BIODB', toupper(gsub('.', '_', key, fixed=TRUE))),
                    collapse='_')
    if (envvar %in% names(.self$.env)) {
        value <- .self$.env[[envvar]]
        logDebug0("Found env var ", envvar, ', value "', value,
            '", defining default value for config key ', key, '.')
    }

    return(value)
},

.newKey=function(key, title, type, default=NULL, description=NA_character_,
    deprecated=NULL) {

    # Check key
    if (is.null(key) || is.na(key) || ! is.character(key))
        error("Key is NULL, NA or not character type.")

    # Check duplicated key
    if (key %in% names(.self$.keys))
        # TODO If key is the same, does not raise error.
        error("Key %s has already been defined in configuration.", key)

    # Overwrite default value by env var, if defined
    env.var.value <- .self$.getFromEnv(key)
    if ( ! is.null(env.var.value))
        default <- env.var.value
    if (is.null(default)) {
        if (key == 'useragent' && 'EMAIL' %in% names(.self$.env))
            default <- paste('Biodb user', .self$.env[['EMAIL']], sep=' ; ')
        if (key == 'svn.binary.path')
            default <- .self$.getSvnBinaryPath()
    }

    # Define new key
    .self$.keys[[key]] <- list(title=title, type=type, default=default,
        description=description)

    # Set as deprecated
    if ( ! is.null(deprecated))
        .self$.keys[[key]][['deprecated']] <- deprecated

    # Initialize value
    if (is.null(deprecated) && ! is.null(default))
        .self$set(key, default)
},

.checkKey=function(key, type=NA_character_, fail=TRUE) {

    # Check key
    if (is.null(key) || is.na(key) || ! is.character(key)) {
        if (fail)
            error("Key is NULL, NA or not character type.")
        else
            return(FALSE)
    }

    # Fail if invalid key
    if ( ! key %in% names(.self$.keys)) {
        if (fail)
            error("Unknown key %s.", key)
        else
            return(FALSE)
    }

    # Fail if deprecated
    if (.self$.isDeprecated(key))
        warn("Key %s is deprecated. %s", key,
            .self$.keys[[key]][['deprecated']])

    # Test type
    if ( ! is.null(type) && ! is.na(type)
        && .self$.keys[[key]][['type']] != type) {
        if (fail)
            error0("Key ", key, " is not of type ", type,
                " but of type ", key.type, ".")
        else
            return(FALSE)
    }

    return(TRUE)
},

.getType=function(key) {

    .self$.checkKey(key)

    return(.self$.keys[[key]][['type']])
},

.isDeprecated=function(key) {
    return('deprecated' %in% names(.self$.keys[[key]]))
}
))
