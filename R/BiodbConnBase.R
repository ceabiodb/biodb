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
#' mybiodb <- biodb::BiodbMain()
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
#' @import methods
#' @include BiodbChildObject.R
#' @export BiodbConnBase
#' @exportClass BiodbConnBase
BiodbConnBase <- methods::setRefClass("BiodbConnBase",
    contains="BiodbChildObject",
    fields=list(
        .db.class="character",
        .observers='list',
        .prop.def='list',
        .prop='list',
        .run.hooks='character'),

methods=list(

initialize=function(other=NULL, db.class=NULL, properties=NULL, ...) {

    callSuper(...)
    .self$.abstractClass('BiodbConnBase')
    .self$.run.hooks <- character()

    # Take parameter values from other object instance
    if ( ! is.null(other)) {
        chk::chk_is(other, "BiodbConnBase")
        for (param in c('db.class'))
            if (is.null(get(param)) || is.na(get(param)))
                assign(param, other[[paste0('.', param)]])
    }

    # Set database class
    chk::chk_string(db.class)
    .self$.db.class <- db.class

    # Set observers
    .self$.observers <- list()

    # Set if it is a remote database connector
    if (methods::extends(.self$getConnClassName(), "BiodbRemotedbConn")
        || ('scheduler.n' %in% names(properties) && ! is.na(properties$scheduler.n))
        || ('scheduler.t' %in% names(properties) && ! is.na(properties$scheduler.t))
        || ('urls' %in% names(properties) && 'base.url' %in% names(properties$url) && length(grep('^http', properties$url$base.url)) > 0))
        properties$remotedb <- TRUE
    
    # Set properties
    .self$.defineProperties(other, properties)
},

show=function() {
    ":\n\nPrints a description of this connector.
    \nReturned value: None.
    "

    # Title
    type <- if (class(.self) %in% c('BiodbConnBase', 'BiodbDbInfo')) "class"
        else "instance"
    cat(.self$getPropertyValue('name'), ' ', type, ".\n", sep='')
    
    # Name / ID
    cat("  Class: ", .self$.db.class, ".\n", sep='')

    # Package
    cat('  Package: ', .self$getPropertyValue('package'), ".\n", sep='')

    # Description
    if (.self$hasProp('description')
        && ! is.na(.self$getPropertyValue('description')))
        cat("  Description: ", .self$getPropertyValue('description'), ".\n",
            sep='')

    # Entry content type
    cat('  Entry content type: ', .self$getPropertyValue('entry.content.type'),
        ".\n", sep='')
    
    # URL
    if (.self$hasProp('urls')) {
        i <- 0
        for (slot in .self$getPropSlots('urls')) {
            cat(if (i == 0) '  URLs: ' else '        ')
            cat(slot, ': ', .self$getPropValSlot('urls', slot), ".\n", sep='')
            i <- i + 1
        }
    }

    # Scheduler parameters
    if (.self$getPropertyValue('remotedb')) {
        st <- .self$getPropertyValue('scheduler.t')
        sn <- .self$getPropertyValue('scheduler.n')
        if ( ! is.na(st) && ! is.na(sn))
            cat('  Request maximum rate: ', sn, ' request(s) every ',
                st, ' second(s)', ".\n", sep='')
    }

    # Disabled
    if (.self$getPropertyValue('disabled')) {
        reason <- .self$getPropertyValue('disabling.reason')
        cat('This connector currently is DISABLED. ', reason, "\n")
    }
},

hasProp=function(name) {
    ":\n\nTests if this connector has a property.
    \nname: The name of the property to check.
    \nReturned value: Returns true if the property `name` exists.
    "

    .self$.checkProperty(name)

    return (name %in% names(.self$.prop))
},

getPropSlots=function(name) {
    ":\n\nGets the slot fields of a property.
    \nname: The name of a property.
    \nReturned value: Returns a character vector containing all slot names
    defined."

    .self$.checkProperty(name, slot=TRUE)

    return (names(.self$.prop[[name]]))
},

hasPropSlot=function(name, slot) {
    ":\n\nTests if a slot property has a specific slot.
    \nname: The name of a property.
    \nslot: The slot name to check.
    \nReturned value: Returns TRUE if the property `name` exists and has the
    slot `slot` defined, and FALSE otherwise."

    .self$.checkProperty(name, slot=slot)

    return (.self$hasProp(name) && slot %in% names(.self$.prop[[name]]))
},

propExists=function(name) {
    ":\n\nChecks if property exists.
    \nname: The name of a property.
    \nReturned value: Returns TRUE if the property `name` exists, and FALSE
    otherwise."

    return(.self$.checkProperty(name, fail=FALSE))
},

isSlotProp=function(name) {
    ":\n\nTests if a property is a slot property.
    \nname: The name of a property.
    \nReturned value: Returns TRUE if the property is a slot propert, FALSE
    otherwise."

    return(.self$.checkProperty(name, slot=TRUE, fail=FALSE))
},

getPropValSlot=function(name, slot) {
    ":\n\nRetrieve the value of a slot of a property.
    \nname: The name of a property.
    \nslot: The slot name inside the property.
    \nReturned value: The value of the slot `slot` of the property `name`.
    "

    value <- .self$getPropertyValue(name)
    .self$.checkProperty(name=name, slot=slot)

    if (slot %in% names(value))
        value <- value[[slot]]
    else {
        pdef <- .self$.prop.def[[name]]
        value <- as.vector(NA, mode=pdef$class)
    }

    return(value)
},

updatePropertiesDefinition=function(def) {
    ":\n\nUpdate the definition of properties.
    \ndef: A named list of property definitions. The names of the list must be
    the property names.
    \nReturned value: None.
    "

    # Loop on properties
    for (prop in names(def)) {

        # Set value to an unset property
        if ( ! prop %in% names(.self$.prop))
            .self$setPropertyValue(prop, def[[prop]])

        # Update value of a slot property
        else if (.self$isSlotProp(prop))
            for (slot in names(def[[prop]]))
                .self$setPropValSlot(prop, slot, def[[prop]][[slot]])

        # Update a single value
        else
            .self$setPropertyValue(prop, def[[prop]])
    }
},

defineParsingExpressions=function() {
    ":\n\nReimplement this method in your connector class to define parsing
    expressions dynamically.
    \nReturned value: None.
    "
},

getEntryFileExt=function() {
    ":\n\nReturns the entry file extension used by this connector.
    \nReturned value: A character value containing the file extension.
    "

    if (.self$getPropertyValue('entry.content.type') == 'list')
        ext <- 'json'
    else
        ext <- .self$getPropertyValue('entry.content.type')

    return(ext)
},

getDbClass=function() {
    ":\n\nGets the Biodb name of the database associated with this connector.
    \nReturned value: A character value containing the Biodb database name.
    "

    return(.self$.db.class)
},

getConnClassName=function() {
    ":\n\nGets the name of the associated connector OOP class.
    \nReturned value: Returns the connector OOP class name.
    "

    return(biodb:::getConnClassName(.self$.db.class))
},

getConnClass=function() {
    ":\n\nGets the associated connector OOP class.
    \nReturned value: Returns the connector OOP class.
    "

    # Load associated package
    pkg <- .self$getPropertyValue('package')
    require(pkg, character.only=TRUE)

    return(get(.self$getConnClassName()))
},

getEntryClassName=function() {
    ":\n\nGets the name of the associated entry class.
    \nReturned value: Returns the name of the associated entry class.
    "

    return(biodb:::getEntryClassName(.self$.db.class))
},

getEntryClass=function() {
    ":\n\nGets the associated entry class.
    \nReturned value: Returns the associated entry class.
    "

    # Load associated package
    pkg <- .self$getPropertyValue('package')
    require(pkg, character.only=TRUE)

    return(get(.self$getEntryClassName()))
},

getEntryIdField=function() {
    ":\n\nGets the name of the corresponding database ID field in entries.
    \nReturned value: Returns the name of the database ID field.
    "

    return(paste(.self$.db.class, 'id', sep='.'))
},

getPropertyValue=function(name) {
    ":\n\nGets a property value.
    \nname: The name of the property.
    \nReturned value: The value of the property.
    "

    .self$.checkProperty(name)
    pdef <- .self$.prop.def[[name]]

    # Run hook
    if ('hook' %in% names(pdef) && ! pdef$hook %in% .self$.run.hooks) {
        .self$.run.hooks <- c(.self$.run.hooks, pdef$hook)
        eval(parse(text=paste0('.self$', pdef$hook, '()')))
    }

    # Get value
    if (name %in% names(.self$.prop))
        value <- .self$.prop[[name]]
    else
        value <- pdef$default

    return(value)
},

setPropertyValue=function(name, value) {
    ":\n\nSets the value of a property.
    \nname: The name of the property.
    \nvalue: The new value to set the property to.
    \nReturned value: None.
    "

    # Check value
    value <- .self$.chkPropVal(name, value)

    # Is this property already set and not modifiable?
    if (name %in% names(.self$.prop)
        && 'modifiable' %in% names(.self$.prop.def[[name]])
        && ! .self$.prop.def[[name]]$modifiable
        && ! identical(.self$.prop[[name]], value))
        error0('Property "', name, '" of database "', .self$getDbClass(),
              '" is not modifiable. Current value is "', .self$.prop[[name]], '". New desired value was "', value, '".')

    # Set value
    .self$.prop[[name]] <- value

    # Notify observers
    for (obs in .self$.observers)
        if (name %in% c('scheduler.n', 'scheduler.t'))
            obs$connSchedulerFrequencyUpdated(.self)
        else if (name == 'urls')
            obs$connUrlsUpdated(.self)
},

setPropValSlot=function(name, slot, value) {
    ":\n\nSet the value of the slot of a property.
    \nname: The name of the property.
    \nslot: The name of the property's slot.
    \nvalue: The new value to set the property's slot to.
    \nReturned value: None.
    "

    .self$.checkProperty(name=name, slot=slot)

    # Get current value
    curval <- .self$getPropertyValue(name)

    # Add/set new value
    curval[[slot]] <- value

    # Update value
    .self$setPropertyValue(name, curval)
},

.terminate=function() {

    # Notify observers
    for (obs in .self$.observers)
        obs$connTerminating(.self)

    # Do terminate (do specific job for the connector)
    .self$.doTerminate()
},

.doTerminate=function() {
},

.registerObserver=function(obs) {

    chk::chk_is(obs, 'BiodbConnObserver')

    # Is this observer already registered?
    if (any(vapply(.self$.observers, function(x) identical(x, obs),
                   FUN.VALUE=TRUE)))
        .self$message('warning', "Observer is already registered.")

    # Register this new observer
    else
        .self$.observers <- c(.self$.observers, obs)
},

.unregisterObserver=function(obs) {

    chk::chk_is(obs, 'BiodbConnObserver')

    # Search for observer
    found.obs <- vapply(.self$.observers, function(x) identical(x, obs),
                        FUN.VALUE=TRUE)

    # Not found
    if ( ! any(found.obs))
        .self$message('warning', 'Unknown observer to unregister.')

    # Unregister observer
    else
        .self$.observers <- .self$.observers[ ! found.obs ]
},

.checkProperty=function(name, slot=NULL, fail=TRUE) {

    # Check that property exists
    if ( ! name %in% names(.self$.prop.def)) {
        if (fail)
            error0('Unknown property "', name, '" for database ',
                  .self$getDbClass(), '.')
        else
            return(FALSE)
    }

    # Get property definition
    pdef <- .self$.prop.def[[name]]

    # Check that it is a property slot
    if (is.logical(slot) && slot && ! 'named' %in% names(pdef)) {
        if (fail)
            error0('Property "', name, '" of database "',
                  .self$getDbClass(), '" is not a slot property.')
        else
            return(FALSE)
    }

    # Check that it is a property slot
    if ( ! is.null(slot) && ! 'named' %in% names(pdef)) {
        if (fail)
            error0('Unauthorized use of slot "', slot,
                  '" with unnamed property "', name, '" of database "',
                  .self$getDbClass(), '".')
        else
            return(FALSE)
    }

    return(if (fail) invisible() else TRUE)
},

.chkPropVal=function(name, value) {

    .self$.checkProperty(name)

    pdef <- .self$.prop.def[[name]]

    # Check cardinality
    if ( ( ! 'mult' %in% names(pdef) || ! pdef$mult) && length(value) > 1)
        error0('Multiple values are forbidden for property "', name,
              '" of database "', .self$getDbClass(), '".')

    # Check names
    if ('named' %in% names(pdef) && ! is.null(value) && length(value) > 0) {
        if (is.null(names(value)) || any(nchar(names(value)) == 0))
            error0('Value vector for property "', name, '"of database "',
                  .self$getDbClass(), '" must be named. Values are: ',
                  paste(paste(names(value), value, sep='='), collapse=', '))
        if (any(duplicated(names(value))))
            error0('Value vector for property "', name, '"of database "',
                  .self$getDbClass(), '" contains duplicated names.')
    }

    # Convert value
    nms <- names(value)
    value <- as.vector(value, mode=pdef$class)
    names(value) <- nms

    # Check if value is allowed
    if (length(value) == 1) {
        if (is.na(value) && 'na.allowed' %in% names(pdef) && ! pdef$na.allowed)
            error0('NA value is not allowed for property "', name,
                  '" of database "', .self$getDbClass(), '".')
        if ( ! is.na(value) && 'allowed' %in% names(pdef)
            && ! value %in% pdef$allowed)
            error0('Value "', value, '" is not allowed for property "',
                  name, '" of database "', .self$getDbClass(), '".')
    }

    return(value)
},

.defineProperties=function(other, properties) {

    # Set list of property definitions
    if (is.null(other))
        .self$.prop.def <- .self$.getFullPropDefList()
    else
        .self$.prop.def <- other$.prop.def

    # Reset default values
    if ( ! is.null(properties))
        for (p in names(properties))
            .self$.prop.def[[p]]$default <- .self$.chkPropVal(p,
                                                              properties[[p]])

    # Set property values
    if (is.null(other))
        .self$.resetPropertyValues()
    else
        .self$.prop <- other$.prop

    # Set chosen values from properties
    if ( ! is.null(properties))
        for (p in names(properties))
            .self$.prop[[p]] <- .self$.chkPropVal(p, properties[[p]])
},

.resetPropertyValues=function() {

    .self$.prop <- list()
    for (p in names(.self$.prop.def))
        .self$setPropertyValue(p, .self$.prop.def[[p]]$default)
},

.getFullPropDefList=function() {

    # Default token
    default_token <- NA_character_
    config <- .self$getBiodb()$getConfig()
    token.key <- paste(.self$getDbClass(), 'token', sep='.')
    if (config$isDefined(token.key, fail=FALSE))
        default_token <- config$get(token.key)

    # Define properties
    prop.def <- list(
        description=list(class='character', default=NA_character_,
                    na.allowed=TRUE, modifiable=FALSE),
        disabled=list(class='logical', default=FALSE, modifiable=TRUE),
        disabling.reason=list(class='character', default=''),
        dwnld.ext=list(class='character', default=NA_character_,
                       modifiable=FALSE),
        entry.content.encoding=list(class='character',
                                      default=NA_character_,
                                      na.allowed=TRUE),
        entry.content.type=list(class='character', default=NA_character_,
                                  allowed=c('html', 'sdf', 'txt', 'xml', 'csv',
                                              'tsv', 'json', 'list'),
                                  na.allowed=FALSE, modifiable=FALSE),
        matching.fields=list(class='list',
                             default=list(mz=c('peak.mztheo',
                                            'peak.mzexp')),
                             named=TRUE, mult=TRUE, na.allowed=FALSE,
                             allowed_item_types='character'),
        name=list(class='character', default=NA_character_,
                    na.allowed=FALSE, modifiable=FALSE),
        package=list(class='character', default='biodb', na.allowed=FALSE,
                     modifiable=FALSE),
        parsing.expr=list(class='list', default=NULL, named=TRUE,
                            mult=TRUE, allowed_item_types='character',
                            na.allowed=FALSE,
                            hook='defineParsingExpressions'),
        remotedb=list(class='logical', default=FALSE, na.allowed=FALSE,
                      modifiable=FALSE),
        searchable.fields=list(class='character', default=character(),
                               na.allowed=FALSE, modifiable=FALSE, mult=TRUE),
        scheduler.n=list(class='integer', default=1, na.allowed=FALSE),
        scheduler.t=list(class='numeric', default=1, na.allowed=FALSE),
        token=list(class='character', default=default_token,
                     na.allowed=TRUE),
        urls=list(class='character', default=character(), named=TRUE,
                    mult=TRUE),
        xml.ns=list(class='character', default=character(), named=TRUE,
                      mult=TRUE)
    )

    return(prop.def)
},

.checkSettingOfUrl=function(key, value) {
    # Accept setting by default
},

getBaseUrl=function() {
    "Returns the base URL."

    lifecycle::deprecate_warn('1.0.0', "getBaseUrl()", "getPropValSlot()")

    return(.self$getPropValSlot('urls', 'base.url'))
},

setBaseUrl=function(url) {
    "Sets the base URL."

    lifecycle::deprecate_warn('1.0.0', "setBaseUrl()", "setPropValSlot()")

    .self$setPropValSlot('urls', 'base.url', url)
},

getWsUrl=function() {
    "Returns the web sevices URL."

    lifecycle::deprecate_warn('1.0.0', "getWsUrl()", "getPropValSlot()")

    return(.self$getPropValSlot('urls', 'ws.url'))
},

setWsUrl=function(ws.url) {
    "Sets the web sevices URL."

    lifecycle::deprecate_warn('1.0.0', "setWsUrl()", "setPropValSlot()")

    .self$setPropValSlot('urls', 'ws.url', ws.url)
},

getToken=function() {
    "Returns the access token."

    lifecycle::deprecate_soft('1.0.0', "getToken()", "getPropertyValue()")

    return(.self$getPropertyValue('token'))
},

setToken=function(token) {
    "Sets the access token."

    lifecycle::deprecate_soft('1.0.0', "setToken()", "setPropertyValue()")

    .self$setPropertyValue('token', token)
},

getName=function() {
    "Returns the full database name."

    lifecycle::deprecate_soft('1.0.0', "getName()", "getPropertyValue()")

    return(.self$getPropertyValue('name'))
},

getEntryContentType=function() {
    "Returns the entry content type."

    lifecycle::deprecate_soft('1.0.0', "getEntryContentType()",
                              "setPropertyValue()")

    return(.self$getPropertyValue('entry.content.type'))
},

getSchedulerNParam=function() {
    "Returns the N parameter for the scheduler."

    lifecycle::deprecate_soft('1.0.0', "getSchedulerNParam()",
                              "getPropertyValue()")

    return(.self$getPropertyValue('scheduler.n'))
},

setSchedulerNParam=function(n) {
    "Sets the N parameter for the scheduler."

    lifecycle::deprecate_soft('1.0.0', "setSchedulerNParam()",
                              "setPropertyValue()")

    .self$setPropertyValue('scheduler.n', n)
},

getSchedulerTParam=function() {
    "Returns the T parameter for the scheduler."


    lifecycle::deprecate_soft('1.0.0', "getSchedulerTParam()",
                              "getPropertyValue()")

    return(.self$getPropertyValue('scheduler.t'))
},

setSchedulerTParam=function(t) {
    "Sets the T parameter for the scheduler."

    lifecycle::deprecate_soft('1.0.0', "setSchedulerTParam()",
                              "setPropertyValue()")

    .self$setPropertyValue('scheduler.t', t)
},

getUrls=function() {
    "Returns the URLs."

    lifecycle::deprecate_soft('1.0.0', "getUrls()", "getPropertyValue()")

    return(.self$getPropertyValue('urls'))
},

getUrl=function(name) {
    "Returns a URL."

    lifecycle::deprecate_soft('1.0.0', "getUrl()", "getPropValSlot()")

    return(.self$getPropValSlot(name='urls', slot=name))
},

setUrl=function(name, url) {
    "Returns a URL."

    lifecycle::deprecate_soft('1.0.0', "setUrl()", "setPropValSlot()")

    .self$setPropValSlot(name='urls', slot=name, value=url)
},

getXmlNs=function() {
    "Returns the XML namespace."

    lifecycle::deprecate_soft('1.0.0', "getXmlNs()", "getPropertyValue()")

    return(.self$getPropertyValue('xml.ns'))
}

))
