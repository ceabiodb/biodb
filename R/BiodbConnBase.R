# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbConnBase {{{1
################################################################################

# Declaration {{{2
################################################################################

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
#' define your own files and loaded them using the \code{Biodb::()} method.
#'
#' Arguments to the contructor are:
#'
#' other: Another object inheriting from \code{BiodbBaseConn}, and
#'        from which property values will be copied.
#'
#' db.class: The class of the database (\code{"mass.csv.file", "chebi",
#'           ...}).
#'
#' properties: Some properties to set at initialization.
#'
#' @seealso \code{\link{BiodbDbsInfo}}, \code{\link{BiodbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#' 
#' # Accessing BiodbConnBase methods when using a BiodbDbInfo object
#' dbinf <- mybiodb$getDbsInfo()$get('chebi')
#'
#' # Test if a property exists
#' dbinf$hasProp('token')
#'
#' # Get a property value
#' dbinf$getPropertyValue('name')
#'
#' # Set a property value
#' dbinf$setPropertyValue('token', 'MyTokenValue')
#'
#' # Get a property value slot
#' dbinf$getPropValSlot('urls', 'base.url')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#' mybiodb <- NULL
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

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(other=NULL, db.class=NULL, properties=NULL, ...) {

    callSuper(...)
    .self$.abstractClass('BiodbConnBase')
    .self$.run.hooks <- character()

    # Take parameter values from other object instance
    if ( ! is.null(other)) {
        .self$.assertInheritsFrom(other, "BiodbConnBase")
        for (param in c('db.class'))
            if (is.null(get(param)) || is.na(get(param)))
                assign(param, other[[paste0('.', param)]])
    }

    # Set database class
    .self$.assertNotNull(db.class)
    .self$.assertNotNa(db.class)
    .self$.assertIs(db.class, 'character')
    .self$.db.class <- db.class

    # Set observers
    .self$.observers <- list()

    # Set properties
    .self$.defineProperties(other, properties)
},

# Show {{{3
################################################################################

show=function() {
    ":\n\nPrints the values of the properties of this connector.
    \nReturned value: None.
    "

    # General info
    msg <- paste0("Biodb ", .self$getPropertyValue('name'),
                  " connector instance")
    if (.self$hasPropSlot('urls', 'base.url'))
        msg <- paste0(msg, ', using URL "',
                      .self$getPropValSlot('urls', 'base.url'), '"')
    msg <- paste0(msg, ".\n")

    # Disabled
    if (.self$getPropertyValue('disabled')) {
        reason <- .self$getPropertyValue('disabling.reason')
        msg <- paste0(msg, 'This connector currently is DISABLED. ',
                      reason, "\n")
    }

    # Print info
    cat(msg)
},

# Has property {{{3
################################################################################

hasProp=function(name) {
    ":\n\nTests if this connector has a property.
    \nname: The name of the property to check.
    \nReturned value: Returns true if the property `name` exists.
    "

    return (name %in% names(.self$.prop))
},

# Has property slot {{{3
################################################################################

hasPropSlot=function(name, slot) {
    ":\n\nTests if a slot property has a specific slot.
    \nname: The name of a property.
    \nslot: The slot name to check.
    \nReturned value: Returns true if the property `name` exists and has the slot `slot`
    defined."

    return (.self$hasProp(name) && slot %in% names(.self$.prop[[name]]))
},

# Get property value slot {{{3
################################################################################

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

# Update properties definition {{{3
################################################################################

updatePropertiesDefinition=function(def) {
    ":\n\nUpdate the definition of properties.
    \ndef: A named list of property definitions. The names of the list must be
    the property names.
    \nReturned value: None.
    "

    # Loop on properties
    for (prop in names(def)) {

        # Set single value
        if ( ! prop %in% names(.self$.prop)
            || is.null(names(def[[prop]])))
            .self$setPropertyValue(def[[prop]])

        # Set named values
        else
            for (slot in names(def[[prop]]))
                .self$setPropValSlot(prop, slot, def[[prop]][[slot]])
    }
},

# Define parsing expressions {{{3
################################################################################

defineParsingExpressions=function() {
    ":\n\nReimplement this method in your connector class to define parsing
    expressions dynamically.
    \nReturned value: None.
    "
},

# Get entry file extension {{{3
################################################################################

getEntryFileExt=function() {
    ":\n\nReturns the entry file extension used by this connector.
    \nReturned value: A character value containing the file extension.
    "

    if (.self$getPropertyValue('entry.content.type') == 'list')
        ext <- 'RData'
    else
        ext <- .self$getPropertyValue('entry.content.type')

    return(ext)
},

# Get database class {{{3
################################################################################

getDbClass=function() {
    ":\n\nGets the Biodb name of the database associated with this connector.
    \nReturned value: A character value containing the Biodb database name.
    "

    return(.self$.db.class)
},

# Get connector class name {{{3
################################################################################

getConnClassName=function() {
    ":\n\nGets the name of the associated connector OOP class.
    \nReturned value: Returns the connector OOP class name.
    "

    # Get connection class name
    s <- .self$.getClassNamePrefix()
    conn.class.name <- paste(s, 'Conn', sep='')

    return(conn.class.name)
},

# Get connector class {{{3
################################################################################

getConnClass=function() {
    ":\n\nGets the associated connector OOP class.
    \nReturned value: Returns the connector OOP class.
    "

    return(get(.self$getConnClassName()))
},


# Get entry class name {{{3
################################################################################

getEntryClassName=function() {
    ":\n\nGets the name of the associated entry class.
    \nReturned value: Returns the name of the associated entry class.
    "

    # Get entry class name
    s <- .self$.getClassNamePrefix()
    entry.class.name <- paste(s, 'Entry', sep='')

    return(entry.class.name)
},

# Get entry class {{{3
################################################################################

getEntryClass=function() {
    ":\n\nGets the associated entry class.
    \nReturned value: Returns the associated entry class.
    "

    return(get(.self$getEntryClassName()))
},

# Get entry ID field {{{3
################################################################################

getEntryIdField=function() {
    ":\n\nGets the name of the corresponding database ID field in entries.
    \nReturned value: Returns the name of the database ID field.
    "

    return(paste(.self$.db.class, 'id', sep='.'))
},

# Get property value {{{3
################################################################################

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

# Set property value {{{3
################################################################################

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
        && ! .self$.prop.def[[name]]$modifiable)
        .self$error('Property "', name, '" of database "', .self$getDbClass(),
                   '" is not modifiable.')

    # Set value
    .self$.prop[[name]] <- value

    # Notify observers
    for (obs in .self$.observers)
        if (name %in% c('scheduler.n', 'scheduler.t'))
            obs$connSchedulerFrequencyUpdated(.self)
        else if (name == 'urls')
            obs$connUrlsUpdated(.self)
},

# Set property value slot {{{3
################################################################################

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

# Private methods {{{2
################################################################################

# Terminate {{{3
################################################################################

.terminate=function() {

    # Notify observers
    for (obs in .self$.observers)
        obs$connTerminating(.self)

    # Do terminate (do specific job for the connector)
    .self$.doTerminate()
},

# Do terminate {{{3
################################################################################

.doTerminate=function() {
},

# Register observer {{{3
################################################################################

.registerObserver=function(obs) {

    .self$.assertInheritsFrom(obs, 'BiodbConnObserver')

    # Is this observer already registered?
    if (any(vapply(.self$.observers, function(x) identical(x, obs),
                   FUN.VALUE=TRUE)))
        .self$message('caution', "Observer is already registered.")

    # Register this new observer
    else
        .self$.observers <- c(.self$.observers, obs)
},

# Unregister observer {{{3
################################################################################

.unregisterObserver=function(obs) {

    .self$.assertInheritsFrom(obs, 'BiodbConnObserver')

    # Search for observer
    found.obs <- vapply(.self$.observers, function(x) identical(x, obs),
                        FUN.VALUE=TRUE)

    # Not found
    if ( ! any(found.obs))
        .self$message('caution', 'Unknown observer to unregister.')

    # Unregister observer
    else
        .self$.observers <- .self$.observers[ ! found.obs ]
},

# Check property {{{3
################################################################################

.checkProperty=function(name, slot=NULL) {

    if ( ! name %in% names(.self$.prop.def))
        .self$error('Unknown property "', name, '" for database ',
                    .self$getDbClass(), '.')

    pdef <- .self$.prop.def[[name]]
    if ( ! is.null(slot) && ! 'named' %in% names(pdef))
        .self$error('Unauthorized use of slot "', slot, '" with unnamed", 
                    " property "', name, '" of database "', .self$getDbClass(),
                    '".')
},

# Check property value {{{3
################################################################################

.chkPropVal=function(name, value) {

    .self$.checkProperty(name)

    pdef <- .self$.prop.def[[name]]

    # Check cardinality
    if ( ( ! 'mult' %in% names(pdef) || ! pdef$mult) && length(value) > 1)
        .self$error('Multiple values are forbidden for property "', name,
                    '" of database "', .self$getDbClass(), '".')

    # Check names
    if ('named' %in% names(pdef) && ! is.null(value) && length(value) > 0) {
        if (is.null(names(value)) || any(nchar(names(value)) == 0))
            .self$error('Value vector for property "', name, '"of database "',
                        .self$getDbClass(), '" must be named. Values are: ',
                        paste(paste(names(value), value, sep='='),
                              collapse=', '))
        if (any(duplicated(names(value))))
            .self$error('Value vector for property "', name, '"of database "',
                        .self$getDbClass(), '" contains duplicated names.')
    }

    # Convert value
    nms <- names(value)
    value <- as.vector(value, mode=pdef$class)
    names(value) <- nms

    # Check if value is allowed
    if (length(value) == 1) {
        if (is.na(value) && 'na.allowed' %in% names(pdef) && ! pdef$na.allowed)
            .self$error('NA value is not allowed for property "', name,
                        '" of database "', .self$getDbClass(), '".')
        if ( ! is.na(value) && 'allowed' %in% names(pdef)
            && ! value %in% pdef$allowed)
            .self$error('Value "', value, '" is not allowed for property "',
                        name, '" of database "', .self$getDbClass(), '".')
    }

    return(value)
},

# Define properties {{{3
################################################################################

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

# Reset propertyValues {{{3
################################################################################

.resetPropertyValues=function() {

    .self$.prop <- list()
    for (p in names(.self$.prop.def))
        .self$setPropertyValue(p, .self$.prop.def[[p]]$default)
},

# Get full list of property definitions {{{3
################################################################################

.getFullPropDefList=function() {

    # Default token
    default_token <- NA_character_
    config <- .self$getBiodb()$getConfig()
    token.key <- paste(.self$getDbClass(), 'token', sep='.')
    if (config$isDefined(token.key, fail=FALSE))
        default_token <- config$get(token.key)

    # Define properties
    prop.def <- list(
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
        name=list(class='character', default=NA_character_,
                    na.allowed=FALSE, modifiable=FALSE),
        parsing.expr=list(class='list', default=NULL, named=TRUE,
                            mult=TRUE, allowed_item_types='character',
                            na.allowed=FALSE,
                            hook='defineParsingExpressions'),
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

# Check setting of URL {{{3
################################################################################

.checkSettingOfUrl=function(key, value) {
    # Accept setting by default
},

# Get class name prefix {{{3
################################################################################

.getClassNamePrefix=function() {

    s <- .self$.db.class
    indices <- as.integer(gregexpr('\\.[a-z]', .self$.db.class,
                                   perl=TRUE)[[1]])
    indices <- indices + 1  # We are interested in the letter after the dot.
    indices <- c(1, indices) # Add first letter.
    for (i in indices)
        s <- paste(substring(s, 1, i - 1), toupper(substring(s, i, i)),
                   substring(s, i + 1), sep='')
    s <- gsub('.', '', s, fixed=TRUE) # Remove dots

    return(s)
},

# Deprecated methods {{{2
################################################################################

# Get base URL {{{3
################################################################################

getBaseUrl=function() {
    "Returns the base URL."

    .self$.deprecatedMethod("getUrl()")

    return(.self$getUrl('base.url'))
},

# Set base URL {{{3
################################################################################

setBaseUrl=function(url) {
    "Sets the base URL."

    .self$.deprecatedMethod("setUrl()")

    .self$setUrl('base.url', url)
},

# Get web sevices URL {{{3
################################################################################

getWsUrl=function() {
    "Returns the web sevices URL."

    .self$.deprecatedMethod("getUrl()")

    return(.self$getUrl('ws.url'))
},

# Set web sevices URL {{{3
################################################################################

setWsUrl=function(ws.url) {
    "Sets the web sevices URL."

    .self$.deprecatedMethod("setUrl()")

    .self$setUrl('ws.url', ws.url)
},

# Get token {{{3
################################################################################

getToken=function() {
    "Returns the access token."

    .self$.deprecatedMethod("getPropertyValue('token')")

    return(.self$getPropertyValue('token'))
},

# Set token {{{3
################################################################################

setToken=function(token) {
    "Sets the access token."

    .self$.deprecatedMethod("setPropertyValue('token', 'my_token_value')")

    .self$setPropertyValue('token', token)
},

# Get name {{{3
################################################################################

getName=function() {
    "Returns the full database name."

    .self$.deprecatedMethod("getPropertyValue('name')")

    return(.self$getPropertyValue('name'))
},

# Get entry content type {{{3
################################################################################

getEntryContentType=function() {
    "Returns the entry content type."

    .self$.deprecatedMethod("getPropertyValue('entry.content.type')")

    return(.self$getPropertyValue('entry.content.type'))
},

# Get scheduler N paramater {{{3
################################################################################

getSchedulerNParam=function() {
    "Returns the N parameter for the scheduler."

    .self$.deprecatedMethod("getPropertyValue('scheduler.n')")

    return(.self$getPropertyValue('scheduler.n'))
},

# Set scheduler N paramater {{{3
################################################################################

setSchedulerNParam=function(n) {
    "Sets the N parameter for the scheduler."

    .self$.deprecatedMethod("setPropertyValue('scheduler.n', n)")

    .self$setPropertyValue('scheduler.n', n)
},

# Get scheduler T paramater {{{3
################################################################################

getSchedulerTParam=function() {
    "Returns the T parameter for the scheduler."

    .self$.deprecatedMethod("getPropertyValue('scheduler.t')")

    return(.self$getPropertyValue('scheduler.t'))
},

# Set scheduler T paramater {{{3
################################################################################

setSchedulerTParam=function(t) {
    "Sets the T parameter for the scheduler."

    .self$.deprecatedMethod("setPropertyValue('scheduler.t', t)")

    .self$setPropertyValue('scheduler.t', t)
},

# Get URLs {{{3
################################################################################

getUrls=function() {
    "Returns the URLs."

    .self$.deprecatedMethod("getPropertyValue('urls')")

    return(.self$getPropertyValue('urls'))
},

# Get a URL {{{3
################################################################################

getUrl=function(name) {
    "Returns a URL."

    .self$.deprecatedMethod("getPropValSlot('urls', 'base.url')")

    return(.self$getPropValSlot(name='urls', slot=name))
},

# Set a URL {{{3
################################################################################

setUrl=function(name, url) {
    "Returns a URL."

    .self$.deprecatedMethod(paste0("setPropValSlot('urls', 'base.url',",
                                    " 'http://my/url')"))

    .self$setPropValSlot(name='urls', slot=name, value=url)

#   .self$.checkSettingOfUrl(name, url)
},

# Get XML namespace {{{3
################################################################################

getXmlNs=function() {
    "Returns the XML namespace."

    .self$.deprecatedMethod("getPropertyValue('xml.ns')")

    return(.self$getPropertyValue('xml.ns'))
}

))
