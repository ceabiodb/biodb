#' Base class of \code{BiodbConn} for encapsulating all needed information for
#' database access.
#'
#' This is the base class for \code{BiodbConn} and \code{BiodbDbInfo}.
#' When defining a new connector class, your class must not inherit from
#' \code{BiodbBaseConn} but at least from \code{BiodbConn} (or
#' \code{BiodbRemoteConn} or any subclass of \code{BiodbConn}).
#' Its main purpose is to store property values. Those values are initialized
#' from YAML files. The default definition file is located inside the package
#' in "inst/definitions.yml" and is loaded at Biodb startup. However you can
#' define your own files and load them using the
#' \code{BiodbMain::loadDefinitions()} method.
#'
#' Arguments to the contructor are:
#'
#' other: Another object inheriting from \code{BiodbBaseConn}, and
#'        from which property values will be copied.
#'
#' db.class: The class of the database (\code{"mass.csv.file"},
#'           \code{"comp.csv.file"}, ...).
#'
#' properties: Some properties to set at initialization.
#'
#' @seealso Sub-classes \code{\link{BiodbDbInfo}} and \code{\link{BiodbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Accessing BiodbConnBase methods when using a BiodbDbInfo object
#' dbinf <- mybiodb$getDbsInfo()$get('comp.csv.file')
#'
#' # Test if a property exists
#' dbinf$hasProp('name')
#'
#' # Get a property value
#' dbinf$getPropertyValue('name')
#'
#' # Get a property value slot
#' dbinf$getPropValSlot('urls', 'base.url')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import R6
#' @export
BiodbConnBase <- R6::R6Class("BiodbConnBase",

public=list(

#' @description
#' New instance initializer. Connector objects must not be created directly.
#' Instead, you create new connector instances through the BiodbFactory
#' instance.
#' @param other Another BiodbConnBase instance as a model from which to
#' copy property values.
#' @param db.class   The class of the connector (i.e.: "mass.csv.file").
#' @param properties Some new values for the properties.
#' @param cfg   The BiodbConfig instance from which will be taken some
#' property values.
#' @return Nothing.
initialize=function(other=NULL, db.class=NULL, properties=NULL, cfg=NULL) {

    abstractClass('BiodbConnBase', self)
    private$runHooks <- character()

    # Take parameter values from other object instance
    if ( ! is.null(other)) {
        chk::chk_is(other, "BiodbConnBase")
        for (param in c('db.class'))
            if (is.null(get(param)) || is.na(get(param)))
                assign(param, other[[paste0('.', param)]])
    }

    # Set database class
    chk::chk_string(db.class)
    private$dbClass <- db.class

    # Set observers
    private$observers <- list()

    # Set properties
    private$defineProperties(other, properties, cfg=cfg)

    return(invisible(NULL))
},

#' @description
#' Prints a description of this connector.
#' @return Nothing.
print=function() {

    # Title
    type <- if (class(self)[[1]] %in% c('BiodbConnBase', 'BiodbDbInfo')) "class"
        else "instance"
    cat(self$getPropertyValue('name'), ' ', type, ".\n", sep='')
    
    # Name / ID
    cat("  Class: ", private$dbClass, ".\n", sep='')

    # Package
    cat('  Package: ', self$getPropertyValue('package'), ".\n", sep='')

    # Description
    if (self$hasProp('description')
        && ! is.na(self$getPropertyValue('description')))
        cat("  Description: ", self$getPropertyValue('description'), ".\n",
            sep='')

    # Entry content type
    cat('  Entry content type: ', self$getPropertyValue('entry.content.type'),
        ".\n", sep='')
    
    # URL
    if (self$hasProp('urls')) {
        i <- 0
        for (slot in self$getPropSlots('urls')) {
            cat(if (i == 0) '  URLs: ' else '        ')
            cat(slot, ': ', self$getPropValSlot('urls', slot), ".\n", sep='')
            i <- i + 1
        }
    }

    # Scheduler parameters
    if (self$getPropertyValue('remote')) {
        st <- self$getPropertyValue('scheduler.t')
        sn <- self$getPropertyValue('scheduler.n')
        if ( ! is.na(st) && ! is.na(sn))
            cat('  Request maximum rate: ', sn, ' request(s) every ',
                st, ' second(s)', ".\n", sep='')
    }

    # Disabled
    if (self$getPropertyValue('disabled')) {
        reason <- self$getPropertyValue('disabling.reason')
        cat('This connector currently is DISABLED. ', reason, "\n")
    }

    return(invisible(NULL))
},

#' @description
#' Tests if this connector has a property.
#' @param name The name of the property to check.
#' @return Returns true if the property `name` exists.
hasProp=function(name) {

    private$checkProperty(name)

    return (name %in% names(private$prop))
},

#' @description
#' Gets the slot fields of a property.
#' @param name The name of a property.
#' @return Returns a character vector containing all slot names
#' defined.
getPropSlots=function(name) {

    private$checkProperty(name, slot=TRUE)

    return (names(private$prop[[name]]))
},

#' @description
#' Tests if a slot property has a specific slot.
#' @param name The name of a property.
#' @param slot The slot name to check.
#' @return Returns TRUE if the property `name` exists and has the
#' slot `slot` defined, and FALSE otherwise."
hasPropSlot=function(name, slot) {

    private$checkProperty(name, slot=slot)

    return (self$hasProp(name) && slot %in% names(private$prop[[name]]))
},

#' @description
#' Checks if property exists.
#' @param name The name of a property.
#' @return Returns TRUE if the property `name` exists, and FALSE
#' otherwise.
propExists=function(name) {

    return(private$checkProperty(name, fail=FALSE))
},

#' @description
#' Tests if a property is a slot property.
#' @param name The name of a property.
#' @return Returns TRUE if the property is a slot propert, FALSE
#' otherwise.
isSlotProp=function(name) {

    return(private$checkProperty(name, slot=TRUE, fail=FALSE))
},

#' @description
#' Retrieve the value of a slot of a property.
#' @param name The name of a property.
#' @param slot The slot name inside the property.
#' @return The value of the slot `slot` of the property `name`.
getPropValSlot=function(name, slot) {

    value <- self$getPropertyValue(name)
    private$checkProperty(name=name, slot=slot)

    if (slot %in% names(value))
        value <- value[[slot]]
    else {
        pdef <- private$propDef[[name]]
        value <- as.vector(NA, mode=pdef$class)
    }

    return(value)
},

#' @description
#' Update the definition of properties.
#' @param def A named list of property definitions. The names of the list must
#' be the property names.
#' @return Nothing.
updatePropertiesDefinition=function(def) {

    # Loop on properties
    for (prop in names(def)) {

        # Set value to an unset property
        if ( ! prop %in% names(private$prop))
            self$setPropertyValue(prop, def[[prop]])

        # Update value of a slot property
        else if (self$isSlotProp(prop))
            for (slot in names(def[[prop]]))
                self$setPropValSlot(prop, slot, def[[prop]][[slot]])

        # Update a single value
        else
            self$setPropertyValue(prop, def[[prop]])
    }

    return(invisible(NULL))
},

#' @description
#' Reimplement this method in your connector class to define parsing
#' expressions dynamically.
#' @return Nothing.
defineParsingExpressions=function() {

    return(invisible(NULL))
},

#' @description
#' Returns the entry file extension used by this connector.
#' @return A character value containing the file extension.
getEntryFileExt=function() {

    if (self$getPropertyValue('entry.content.type') == 'list')
        ext <- 'json'
    else
        ext <- self$getPropertyValue('entry.content.type')

    return(ext)
},

#' @description
#' Gets the Biodb name of the database associated with this connector.
#' @return A character value containing the Biodb database name.
getDbClass=function() {

    return(private$dbClass)
},

#' @description
#' Gets the name of the associated connector OOP class.
#' @return Returns the connector OOP class name.
getConnClassName=function() {

    return(biodb:::getConnClassName(private$dbClass))
},

#' @description
#' Gets the associated connector OOP class.
#' @return Returns the connector OOP class.
getConnClass=function() {

    # Load associated package
    pkg <- self$getPropertyValue('package')
    require(pkg, character.only=TRUE)

    return(get(self$getConnClassName()))
},

#' @description
#' Gets the name of the associated entry class.
#' @return Returns the name of the associated entry class.
getEntryClassName=function() {

    return(biodb:::getEntryClassName(private$dbClass))
},

#' @description
#' Gets the associated entry class.
#' @return Returns the associated entry class.
getEntryClass=function() {

    # Load associated package
    pkg <- self$getPropertyValue('package')
    require(pkg, character.only=TRUE)

    return(get(self$getEntryClassName()))
},

#' @description
#' Gets the name of the corresponding database ID field in entries.
#' @return Returns the name of the database ID field.
getEntryIdField=function() {

    return(paste(private$dbClass, 'id', sep='.'))
},

#' @description
#' Gets a property value.
#' @param name The name of the property.
#' @return The value of the property.
getPropertyValue=function(name) {

    private$checkProperty(name)
    pdef <- private$propDef[[name]]

    # Run hook
    if ('hook' %in% names(pdef) && ! pdef$hook %in% private$runHooks) {
        private$runHooks <- c(private$runHooks, pdef$hook)
        eval(parse(text=paste0('self$', pdef$hook, '()')))
    }

    # Get value
    if (name %in% names(private$prop))
        value <- private$prop[[name]]
    else
        value <- pdef$default

    return(value)
},

#' @description
#' Sets the value of a property.
#' @param name The name of the property.
#' @param value The new value to set the property to.
#' @return Nothing.
setPropertyValue=function(name, value) {

    logDebug('Setting property "%s" to "%s".', name, value)

    # Check value
    value <- private$chkPropVal(name, value)

    # Is this property already set and not modifiable?
    if (name %in% names(private$prop)
        && 'modifiable' %in% names(private$propDef[[name]])
        && ! private$propDef[[name]]$modifiable
        && ! identical(private$prop[[name]], value))
        error0('Property "', name, '" of database "', self$getDbClass(),
            '" is not modifiable. Current value is "', private$prop[[name]],
            '". New desired value was "', value, '".')

    # Set value
    private$prop[[name]] <- value

    # Notify observers
    if (name %in% c('scheduler.n', 'scheduler.t')) {
        logDebug("Notifying observers about frequency change.")
        notifyObservers(private$observers,
            'notifyConnSchedulerFrequencyUpdated', conn=self)
    }
    else if (name == 'urls') {
        logDebug("Notifying observers about URLs change.")
        notifyObservers(private$observers, 'notifyConnUrlsUpdated',
            conn=self)
    }

    return(invisible(NULL))
},

#' @description
#' Set the value of the slot of a property.
#' @param name The name of the property.
#' @param slot The name of the property's slot.
#' @param value The new value to set the property's slot to.
#' @return Nothing.
setPropValSlot=function(name, slot, value) {

    private$checkProperty(name=name, slot=slot)

    # Get current value
    curval <- self$getPropertyValue(name)

    # Add/set new value
    curval[[slot]] <- value

    # Update value
    self$setPropertyValue(name, curval)

    return(invisible(NULL))
},

#' @description
#' Returns the base URL.
#' @return THe baae URL.
getBaseUrl=function() {

    lifecycle::deprecate_warn('1.0.0', "getBaseUrl()", "getPropValSlot()")

    return(self$getPropValSlot('urls', 'base.url'))
},

#' @description
#' Sets the base URL.
#' @param url A URL as a character value.
#' @return Nothing.
setBaseUrl=function(url) {

    lifecycle::deprecate_warn('1.0.0', "setBaseUrl()", "setPropValSlot()")

    self$setPropValSlot('urls', 'base.url', url)

    return(invisible(NULL))
},

#' @description
#' Returns the web sevices URL.
getWsUrl=function() {

    lifecycle::deprecate_warn('1.0.0', "getWsUrl()", "getPropValSlot()")

    return(self$getPropValSlot('urls', 'ws.url'))
},

#' @description
#' Sets the web sevices URL.
#' @param ws.url A URL as a character value.
#' @return Nothing.
setWsUrl=function(ws.url) {

    lifecycle::deprecate_warn('1.0.0', "setWsUrl()", "setPropValSlot()")

    self$setPropValSlot('urls', 'ws.url', ws.url)

    return(invisible(NULL))
},

#' @description
#' Returns the access token.
getToken=function() {

    lifecycle::deprecate_soft('1.0.0', "getToken()", "getPropertyValue()")

    return(self$getPropertyValue('token'))
},

#' @description
#' Sets the access token.
#' @param token The token to use to access the database, as a character value.
#' @return Nothing.
setToken=function(token) {

    lifecycle::deprecate_soft('1.0.0', "setToken()", "setPropertyValue()")

    self$setPropertyValue('token', token)

    return(invisible(NULL))
},

#' @description
#' Returns the full database name.
getName=function() {

    lifecycle::deprecate_soft('1.0.0', "getName()", "getPropertyValue()")

    return(self$getPropertyValue('name'))
},

#' @description
#' Returns the entry content type.
getEntryContentType=function() {

    lifecycle::deprecate_soft('1.0.0', "getEntryContentType()",
        "setPropertyValue()")

    return(self$getPropertyValue('entry.content.type'))
},

#' @description
#' Returns the N parameter for the scheduler.
getSchedulerNParam=function() {

    lifecycle::deprecate_soft('1.0.0', "getSchedulerNParam()",
        "getPropertyValue()")

    return(self$getPropertyValue('scheduler.n'))
},

#' @description
#' Sets the N parameter for the scheduler.
#' @param n The N parameter as a whole number.
#' @return Nothing.
setSchedulerNParam=function(n) {

    lifecycle::deprecate_soft('1.0.0', "setSchedulerNParam()",
        "setPropertyValue()")

    self$setPropertyValue('scheduler.n', n)

    return(invisible(NULL))
},

#' @description
#' Returns the T parameter for the scheduler.
getSchedulerTParam=function() {

    lifecycle::deprecate_soft('1.0.0', "getSchedulerTParam()",
        "getPropertyValue()")

    return(self$getPropertyValue('scheduler.t'))
},

#' @description
#' Sets the T parameter for the scheduler.
#' @param t The T parameter as a whole number.
#' @return Nothing.
setSchedulerTParam=function(t) {

    lifecycle::deprecate_soft('1.0.0', "setSchedulerTParam()",
        "setPropertyValue()")

    self$setPropertyValue('scheduler.t', t)

    return(invisible(NULL))
},

#' @description
#' Returns the URLs.
getUrls=function() {

    lifecycle::deprecate_soft('1.0.0', "getUrls()", "getPropertyValue()")

    return(self$getPropertyValue('urls'))
},

#' @description
#' Returns a URL.
#' @param name The name of the URL to retrieve.
#' @return The URL as a character value.
getUrl=function(name) {

    lifecycle::deprecate_soft('1.0.0', "getUrl()", "getPropValSlot()")

    return(self$getPropValSlot(name='urls', slot=name))
},

#' @description
#' Sets a URL.
#' @param name The name of the URL to set.
#' @param url The URL value.
#' @return Nothing.
setUrl=function(name, url) {

    lifecycle::deprecate_soft('1.0.0', "setUrl()", "setPropValSlot()")

    self$setPropValSlot(name='urls', slot=name, value=url)

    return(invisible(NULL))
},

#' @description
#' Returns the XML namespace.
getXmlNs=function() {

    lifecycle::deprecate_soft('1.0.0', "getXmlNs()", "getPropertyValue()")

    return(self$getPropertyValue('xml.ns'))
}

),
private=list(
    dbClass="character"
    ,observers='list'
    ,propDef='list'
    ,prop='list'
    ,runHooks='character'

,checkSettingOfUrl=function(key, value) {
    # Accept setting by default
}

,registerObserver=function(obs) {

    chk::chk_not_null(obs)

    # Is this observer already registered?
    if (any(vapply(private$observers, function(x) identical(x, obs),
        FUN.VALUE=TRUE)))
        biodb::warn("Observer is already registered.")

    # Register this new observer
    else
        private$observers <- c(private$observers, obs)
}

,unregisterObserver=function(obs) {

    chk::chk_not_null(obs)

    # Search for observer
    found.obs <- vapply(private$observers, function(x) identical(x, obs),
        FUN.VALUE=TRUE)

    # Not found
    if ( ! any(found.obs))
        biodb::warn('Unknown observer to unregister.')

    # Unregister observer
    else
        private$observers <- private$observers[ ! found.obs ]
}

,checkProperty=function(name, slot=NULL, fail=TRUE) {

    # Check that property exists
    if ( ! name %in% names(private$propDef)) {
        if (fail)
            error0('Unknown property "', name, '" for database ',
            self$getDbClass(), '.')
        else
            return(FALSE)
    }

    # Get property definition
    pdef <- private$propDef[[name]]

    # Check that it is a property slot
    if (is.logical(slot) && slot && ! 'named' %in% names(pdef)) {
        if (fail)
            error0('Property "', name, '" of database "',
            self$getDbClass(), '" is not a slot property.')
        else
            return(FALSE)
    }

    # Check that it is a property slot
    if ( ! is.null(slot) && ! 'named' %in% names(pdef)) {
        if (fail)
            error0('Unauthorized use of slot "', slot,
                '" with unnamed property "', name, '" of database "',
                self$getDbClass(), '".')
        else
            return(FALSE)
    }

    return(if (fail) invisible() else TRUE)
}

,chkPropVal=function(name, value) {

    private$checkProperty(name)

    pdef <- private$propDef[[name]]

    # Check cardinality
    if ( ( ! 'mult' %in% names(pdef) || ! pdef$mult) && length(value) > 1)
        error0('Multiple values are forbidden for property "', name,
            '" of database "', self$getDbClass(), '".')

    # Check names
    if ('named' %in% names(pdef) && ! is.null(value) && length(value) > 0) {
        if (is.null(names(value)) || any(nchar(names(value)) == 0))
            error0('Value vector for property "', name, '"of database "',
                self$getDbClass(), '" must be named. Values are: ',
                paste(paste(names(value), value, sep='='), collapse=', '))
        if (any(duplicated(names(value))))
            error0('Value vector for property "', name, '"of database "',
                self$getDbClass(), '" contains duplicated names.')
    }

    # Convert value
    nms <- names(value)
    value <- as.vector(value, mode=pdef$class)
    names(value) <- nms

    # Check if value is allowed
    if (length(value) == 1) {
        if (is.na(value) && 'na.allowed' %in% names(pdef) && ! pdef$na.allowed)
            error0('NA value is not allowed for property "', name,
                '" of database "', self$getDbClass(), '".')
        if ( ! is.na(value) && 'allowed' %in% names(pdef)
            && ! value %in% pdef$allowed)
            error0('Value "', value, '" is not allowed for property "',
                name, '" of database "', self$getDbClass(), '".')
    }

    return(value)
}

,defineProperties=function(other, properties, cfg) {

    # Set list of property definitions
    if (is.null(other))
        private$propDef <- private$getFullPropDefList(cfg)
    else
        private$propDef <- other$.__enclos_env__$private$propDef

    # Reset default values
    if ( ! is.null(properties))
        for (p in names(properties))
            private$propDef[[p]]$default <- private$chkPropVal(p,
                properties[[p]])

    # Set property values
    if (is.null(other))
        private$resetPropertyValues()
    else
        private$prop <- other$.__enclos_env__$private$prop

    # Set chosen values from properties
    if ( ! is.null(properties))
        for (p in names(properties))
            private$prop[[p]] <- private$chkPropVal(p, properties[[p]])
}

,resetPropertyValues=function() {

    private$prop <- list()
    for (p in names(private$propDef))
        self$setPropertyValue(p, private$propDef[[p]]$default)
}

,getFullPropDefList=function(cfg) {

    # Default token
    default_token <- NA_character_
    token.key <- paste(self$getDbClass(), 'token', sep='.')
    if (cfg$isDefined(token.key, fail=FALSE))
        default_token <- cfg$get(token.key)

    # Define properties
    prop.def <- list(
        compound.db=list(class='logical', default=FALSE, na.allowed=FALSE,
            modifiable=FALSE),
        description=list(class='character', default=NA_character_,
                    na.allowed=TRUE, modifiable=FALSE),
        disabled=list(class='logical', default=FALSE, modifiable=TRUE),
        disabling.reason=list(class='character', default=''),
        downloadable=list(class='logical', default=FALSE, na.allowed=FALSE,
            modifiable=FALSE),
        dwnld.ext=list(class='character', default=NA_character_,
            modifiable=FALSE),
        editable=list(class='logical', default=FALSE, na.allowed=FALSE,
            modifiable=FALSE),
        entry.content.encoding=list(class='character',
            default=NA_character_, na.allowed=TRUE),
        entry.content.type=list(class='character', default=NA_character_,
            allowed=c('html', 'sdf', 'txt', 'xml', 'csv',
            'tsv', 'json', 'list'),
            na.allowed=FALSE, modifiable=FALSE),
        matching.fields=list(class='list',
            default=list(mz=c('peak.mztheo', 'peak.mzexp')),
            named=TRUE, mult=TRUE, na.allowed=FALSE,
            allowed_item_types='character'),
        mass.db=list(class='logical', default=FALSE, na.allowed=FALSE,
            modifiable=FALSE),
        name=list(class='character', default=NA_character_,
            na.allowed=FALSE, modifiable=FALSE),
        package=list(class='character', default='biodb', na.allowed=FALSE,
            modifiable=FALSE),
        parsing.expr=list(class='list', default=NULL, named=TRUE,
            mult=TRUE, allowed_item_types='character',
            na.allowed=FALSE, hook='defineParsingExpressions'),
        remote=list(class='logical', default=FALSE, na.allowed=FALSE,
            modifiable=FALSE),
        searchable.fields=list(class='character', default=character(),
            na.allowed=FALSE, modifiable=FALSE, mult=TRUE),
        scheduler.n=list(class='integer', default=1, na.allowed=FALSE),
        scheduler.t=list(class='numeric', default=1, na.allowed=FALSE),
        token=list(class='character', default=default_token,
            na.allowed=TRUE),
        urls=list(class='character', default=character(), named=TRUE,
            mult=TRUE),
        writable=list(class='logical', default=FALSE, na.allowed=FALSE,
            modifiable=FALSE),
        xml.ns=list(class='character', default=character(), named=TRUE,
            mult=TRUE)
    )

    return(prop.def)
}
))
