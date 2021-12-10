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
#' @import R6
#' @import rappdirs
#' @export
BiodbConfig <- R6::R6Class("BiodbConfig",

public=list(

#' @description
#' New instance initializer. No BiodbConfig object must not be created directly.
#' Instead, access the config instance through the BiodbMain instance using the
#' getConfig() method.
#' @param parent The BiodbMain instance.
#' @return Nothing.
initialize=function(parent) {

    private$parent <- parent
    private$env <- Sys.getenv()
    private$keys <- list()
    private$values <- list()

    # Register as observer
    private$parent$addObservers(self)

    return(invisible(NULL))
},

#' @description
#' Get the list of available keys.
#' @param deprecated If set to TRUE returns also the deprecated keys.
#' @return A character vector containing the config key names.
getKeys=function(deprecated=FALSE) {

    keys <- Filter(function(k) { ! private$isDeprecated(k) },
        names(private$keys))

    return(keys)
},

#' @description
#' Get the title of a key.
#' @param key The name of a configuration key.
#' @return The title of the key as a character value.
getTitle=function(key) {

    title <- ''

    private$checkKey(key)

    if ('title' %in% names(private$keys[[key]]) &&
        length(private$keys[[key]][['title']]) > 0)
        title <- private$keys[[key]][['title']]

    return(title)
},

#' @description
#' Get the description of a key.
#' @param key The name of a configuration key.
#' @return The description of the key as a character value.
getDescription=function(key) {

    description <- ''

    private$checkKey(key)

    if ('description' %in% names(private$keys[[key]]))
        description <- private$keys[[key]][['description']]

    return(description)
},

#' @description
#' Get the default value of a key.
#' @param key The name of a configuration key.
#' @param as.chr If set to TRUE, returns the value as character.
#' @return The default value for that key.
getDefaultValue=function(key, as.chr=FALSE) {

    default <- NULL

    private$checkKey(key)

    # Get default value
    if ('default' %in% names(private$keys[[key]]))
        default <- private$keys[[key]][['default']]

    if (as.chr)
        default <- if (is.null(default)) NA_character_ else
            as.character(default)

    return(default)
},

#' @description
#' Test if a key exists.
#' @param key The name of a configuration key.
#' @return TRUE if a key with this name exists, FALSE otherwise.
hasKey=function(key) {

    return(private$checkKey(key, fail=FALSE))
},

#' @description
#' Test if a key is defined (i.e.: if a value exists for this key).
#' @param key The name of a configuration key.
#' @param fail If set to TRUE and the configuration key does not exist, then an
#'     error will be raised.
#' @return TRUE if the key has a value, FALSE otherwise.
isDefined=function(key, fail=TRUE) {

    if (private$checkKey(key, fail=fail))
        return(key %in% names(private$values))

    return(FALSE)
},

#' @description
#' Test if a boolean key is set to TRUE. This method will raise an error
#'     if the key is not a boolean key.
#' @param key The name of a configuration key.
#' @return TRUE if the boolean key has a value set to TRUE, FALSE
#'     otherwise.
isEnabled=function(key) {

    private$checkKey(key, type='logical')

    value <- FALSE

    # Defined ?
    if (self$isDefined(key))
        value <- private$values[[key]]

    return(value)
},

#' @description
#' Get the value of a key.
#' @param key The name of a configuration key.
#' @return The value associated with the key.
get=function(key) {

    private$checkKey(key)

    # Is value defined ?
    if (self$isDefined(key))
        value <- private$values[[key]]
    else
        value <- as.vector(NA, mode=private$getType(key))

    return(value)
},

#' @description
#' Set the value of a key.
#' @param key The name of a configuration key.
#' @param value A value to associate with the key.
#' @return Nothing.
set=function(key, value) {

    private$checkKey(key)

    v <- as.vector(value, mode=private$getType(key))
    private$values[[key]] <- v
    displayed.value <- if (is.character(value)) paste0('"', value, '"')
        else value
    logDebug("Set key %s to %s.", key, displayed.value)

    # Notify observers
    notifyObservers(private$parent$getObservers(), 'notifyCfgUpdate', k=key,
        v=v)

    return(invisible(NULL))
},

#' @description
#' Reset the value of a key.
#' @param key The name of a configuration key. If NULL, all keys will be reset.
#' @return Nothing.
reset=function(key=NULL) {

    # Set keys to reset
    if (is.null(key))
        keys <- names(private$keys)
    else {
        private$checkKey(key)
        keys <- key
    }

    # Loop on all keys
    for (k in keys)
        self$set(k, private$keys[[key]]$default)

    return(invisible(NULL))
},

#' @description
#' Set a boolean key to TRUE.
#' @param key The name of a configuration key.
#' @return Nothing.
enable=function(key) {

    private$checkKey(key, type='logical')

    logInfo("Enable %s.", key)
    private$values[[key]] <- TRUE

    return(invisible(NULL))
},

#' @description
#' Set a boolean key to FALSE.
#' @param key The name of a configuration key.
#' @return Nothing.
disable=function(key) {

    private$checkKey(key, type='logical')

    logInfo("Disable %s.", key)
    private$values[[key]] <- FALSE

    return(invisible(NULL))
},

#' @description
#' Print list of configuration keys and their values.
#' @return Nothing.
print=function() {

    cat("Biodb configuration instance.\n")

    # Loop on all keys
    keys <- sort(self$getKeys())
    if (length(keys) > 0) {
        cat("  Values:\n")
        for (key in keys)
            if ( ! private$isDeprecated(key))
                cat("    ", key, ": ", self$get(key), "\n")
    }

    return(invisible(NULL))
},

#' @description
#' Get the full list of keys as a data frame.
#' @return A data frame containing keys, titles, types, and default
#'     values.
listKeys=function() {

    keys <- self$getKeys()
    x <- data.frame(key=keys, stringsAsFactors=FALSE)
    x$title <- vapply(keys, function(k) self$getTitle(k), FUN.VALUE='')
    x$type <- vapply(keys, function(k) private$getType(k), FUN.VALUE='')
    x$default <- vapply(keys,
                        function(k) self$getDefaultValue(k, as.chr=TRUE),
                        FUN.VALUE='')

    return(x)
},

#' @description
#' Returns the environment variable associated with this configuration
#'     key.
#' @param key The name of a configuration key.
#' @return The environment variable's value. 
getAssocEnvVar=function(key) {

    # Check key
    private$checkKey(key)

    # Build env var
    env.var <- paste(c('BIODB', toupper(gsub('.', '_', key, fixed=TRUE))),
        collapse='_')

    return(env.var)
},

#' @description
#' Defines config properties from a structured object, normally loaded
#'     from a YAML file.
#' @param def The list of key definitions.
#' @return Nothing.
define=function(def) {

    # Get key names
    keys <- names(def)

    # Loop on all keys
    for (key in keys) {

        v <- def[[key]]
        v$key <- key
        logDebug('Define config key %s.', key)
        do.call(private$newKey, v)
    }

    return(invisible(NULL))
},

#' @description
#' Called by BiodbMain when a new observer is registered.
#' @param obs The new observers registered by the BiodbMain instance.
#' @return Nothing.
notifyNewObservers=function(obs) {

    # Loop on all keys
    for(key in names(private$values))
        notifyObservers(private$parent$getObservers(), 'notifyCfgUpdate',
            k=key, v=private$values[[key]])

    return(invisible(NULL))
},

#' @description
#' Terminates the instance. This method will be called
#'     automatically by the BiodbMain instance when you call
#' @param BiodbMain :terminate().
#' @return Nothing.
terminate=function() {

    return(invisible(NULL))
}
),

private=list(
    values=NULL,
    env=NULL,
    keys=NULL,
    parent=NULL,

getSvnBinaryPath=function() {

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

getFromEnv=function(key) {

    value <- NULL

    # Look into ENV
    envvar <- paste(c('BIODB', toupper(gsub('.', '_', key, fixed=TRUE))),
                    collapse='_')
    if (envvar %in% names(private$env)) {
        value <- private$env[[envvar]]
        logDebug0("Found env var ", envvar, ', value "', value,
            '", defining default value for config key ', key, '.')
    }

    return(value)
},

newKey=function(key, title, type, default=NULL, description=NA_character_,
    deprecated=NULL) {

    # Check key
    if (is.null(key) || is.na(key) || ! is.character(key))
        error("Key is NULL, NA or not character type.")

    # Check duplicated key
    if (key %in% names(private$keys))
        # TODO If key is the same, does not raise error.
        error("Key %s has already been defined in configuration.", key)

    # Overwrite default value by env var, if defined
    env.var.value <- private$getFromEnv(key)
    if ( ! is.null(env.var.value))
        default <- env.var.value
    if (is.null(default)) {
        if (key == 'useragent') {
            if ('EMAIL' %in% names(private$env))
                default <- paste('Biodb user', private$env[['EMAIL']],
                    sep=' ; ')
            else
                default <- "R Bioconductor biodb library."
        }
        else if (key == 'svn.binary.path')
            default <- private$getSvnBinaryPath()
    }

    # Define new key
    private$keys[[key]] <- list(title=title, type=type, default=default,
        description=description)

    # Set as deprecated
    if ( ! is.null(deprecated))
        private$keys[[key]][['deprecated']] <- deprecated

    # Initialize value
    if (is.null(deprecated) && ! is.null(default))
        self$set(key, default)
},

checkKey=function(key, type=NA_character_, fail=TRUE) {

    # Check key
    if (is.null(key) || is.na(key) || ! is.character(key)) {
        if (fail)
            error("Key is NULL, NA or not character type.")
        else
            return(FALSE)
    }

    # Fail if invalid key
    if ( ! key %in% names(private$keys)) {
        if (fail)
            error("Unknown key %s.", key)
        else
            return(FALSE)
    }

    # Fail if deprecated
    if (private$isDeprecated(key))
        warn("Key %s is deprecated. %s", key,
            private$keys[[key]][['deprecated']])

    # Test type
    if ( ! is.null(type) && ! is.na(type)
        && private$keys[[key]][['type']] != type) {
        if (fail)
            error0("Key ", key, " is not of type ", type,
                " but of type ", key.type, ".")
        else
            return(FALSE)
    }

    return(TRUE)
},

getType=function(key) {

    private$checkKey(key)

    return(private$keys[[key]][['type']])
},

isDeprecated=function(key) {
    return('deprecated' %in% names(private$keys[[key]]))
}
))
