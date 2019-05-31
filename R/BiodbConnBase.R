# vi: fdm=marker

# Class declaration {{{1
################################################################

#' Base class of \code{BiodbConn} for encapsulating all needed information for database access.
#'
#' This is the base class for \code{BiodbConn} and \code{BiodbDbInfo}. The constructor is not meant to be used, but for development purposes the constructor's parameters are nevertheless described in the Fields section.
#'
#' @field db.class      The class of the database (\code{"massbank", "hmdb.metabolies", ...}).
#' @field prop.def      Definitions of the properties.
#' @field prop          Values of the properties.
#'
#' @seealso \code{\link{BiodbDbsInfo}}, \code{\link{BiodbConn}}.
#'
#' @import methods
#' @include BiodbChildObject.R
#' @export BiodbConnBase
#' @exportClass BiodbConnBase
BiodbConnBase <- methods::setRefClass("BiodbConnBase",
	contains =  "BiodbChildObject",
	fields = list(
		.db.class = "character",
		.observers = 'list',
		.prop.def = 'list',
		.prop = 'list',
		.run.hooks = 'character'),

	methods = list(

# Public methods {{{1
################################################################################

# Constructor {{{2
################################################################

initialize = function(other = NULL, db.class = NULL, properties = NULL, ...) {

	callSuper(...)
	.self$.abstract.class('BiodbConnBase')
	.self$.run.hooks <- character()

	# Take parameter values from other object instance
	if ( ! is.null(other)) {
		.self$.assert.inherits.from(other, "BiodbConnBase")
		for (param in c('db.class'))
			if (is.null(get(param)) || is.na(get(param)))
				assign(param, other[[paste0('.', param)]])
	}

	# Set database class
	.self$.assert.not.null(db.class)
	.self$.assert.not.na(db.class)
	.self$.assert.is(db.class, 'character')
	.self$.db.class <- db.class

	# Set observers
	.self$.observers <- list()

	# Set properties
	.self$.defineProperties(other, properties)
},

# Show {{{2
################################################################

show = function() {
	msg <- paste0("Biodb ", .self$getPropertyValue('name'), " connector instance")
	if (.self$hasPropSlot('urls', 'base.url'))
		msg <- paste0(msg, ', using URL "', .self$getPropValSlot('urls', 'base.url'), '"')
	msg <- paste0(msg, ".\n")
	cat(msg)
},

# Has property {{{2
################################################################

hasProp = function(name) {
	'Returns true if the property "name" exists.'

	return (name %in% names(.self$.prop))
},

# Has property slot {{{2
################################################################

hasPropSlot = function(name, slot) {
	'Returns true if the property "name" exists and has the slot "slot" defined.'

	return (.self$hasProp(name) && slot %in% names(.self$.prop[[name]]))
},

# Get property value slot {{{2
################################################################

getPropValSlot = function(name, slot) {
	'Return the value of the slot "slot" of the property "name".'

	value <- .self$getPropertyValue(name)
	.self$.checkProperty(name = name, slot = slot)

	if (slot %in% names(value))
		value <- value[[slot]]
	else {
		pdef <- .self$.prop.def[[name]]
		value <- as.vector(NA, mode = pdef$class)
	}

	return(value)
},

# Update properties definition {{{2
################################################################################

updatePropertiesDefinition = function(def) {
	'Update the definition of properties.'

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

# Define parsing expressions {{{2
################################################################################

defineParsingExpressions = function() {
	'Reimplement this method in your connector class to define parsing expressions dynamically.'
}

))

# Get entry file extension {{{1
################################################################

BiodbConnBase$methods( getEntryFileExt = function() {
	":\n\nReturns the entry file extension."

	if (.self$getPropertyValue('entry.content.type') == 'list')
		ext = 'RData'
	else
		ext = .self$getPropertyValue('entry.content.type')

	return(ext)
})

# Get database class {{{1
################################################################

BiodbConnBase$methods( getDbClass = function() {
	return(.self$.db.class)
})

# Get connector class name {{{1
################################################################

BiodbConnBase$methods( getConnClassName = function() {
	":\n\nReturns the name of the associated connector OOP class."

	# Get connection class name
	s <- .self$.getClassNamePrefix()
	conn.class.name <- paste(s, 'Conn', sep = '')

	return(conn.class.name)
})

# Get entry class name {{{1
################################################################

BiodbConnBase$methods( getEntryClassName = function() {
	":\n\nReturns the name of the associated entry class."

	# Get entry class name
	s <- .self$.getClassNamePrefix()
	entry.class.name <- paste(s, 'Entry', sep = '')

	return(entry.class.name)
})

# Get connector class {{{1
################################################################

BiodbConnBase$methods( getConnClass = function() {
	":\n\nReturns the associated connector OOP class."

	return(get(.self$getConnClassName()))
})

# Get entry class {{{1
################################################################

BiodbConnBase$methods( getEntryClass = function() {
	":\n\nReturns the associated entry class."

	return(get(.self$getEntryClassName()))
})

# Get entry class name {{{1
################################################################

BiodbConnBase$methods( getEntryClassName = function() {
	":\n\nReturns the name of the associated entry class."

    # Get entry class name
	s <- .self$.db.class
	indices <- as.integer(gregexpr('\\.[a-z]', .self$.db.class, perl = TRUE)[[1]])
	indices <- indices + 1  # We are interested in the letter after the dot.
	indices <- c(1, indices) # Add first letter.
	for (i in indices)
		s <- paste(substring(s, 1, i - 1), toupper(substring(s, i, i)), substring(s, i + 1), sep = '')
	s <- gsub('.', '', s, fixed = TRUE) # Remove dots
	entry.class.name <- paste(s, 'Entry', sep = '')

	return(entry.class.name)
})

# Get entry class {{{1
################################################################

BiodbConnBase$methods( getEntryClass = function() {
	":\n\nReturns the associated entry class."

	return(get(.self$getEntryClassName()))
})

# Get entry ID field {{{1
################################################################

BiodbConnBase$methods( getEntryIdField = function() {
	":\n\nReturn the name of the corresponding database ID field in entries."
	
	return(paste(.self$.db.class, 'id', sep = '.'))
})

# Get property value {{{1
################################################################

BiodbConnBase$methods( getPropertyValue = function(name) {

	.self$.checkProperty(name)
	pdef <- .self$.prop.def[[name]]

	# Run hook
	if ('hook' %in% names(pdef) && ! pdef$hook %in% .self$.run.hooks) {
		.self$.run.hooks <- c(.self$.run.hooks, pdef$hook)
		eval(parse(text = paste0('.self$', pdef$hook, '()')))
	}

	# Get value
	if (name %in% names(.self$.prop))
		value <- .self$.prop[[name]]
	else
		value <- pdef$default

	return(value)
})

# Set property value {{{1
################################################################

BiodbConnBase$methods( setPropertyValue = function(name, value) {

	# Check value
	value <- .self$.checkPropertyValue(name, value)

	# Is this property already set and not modifiable?
	if (name %in% names(.self$.prop)
		&& 'modifiable' %in% names(.self$.prop.def[[name]])
	    && ! .self$.prop.def[[name]]$modifiable)
		.self$message('error', paste0('Property "', name, '" of database "', .self$getDbClass(), '" is not modifiable.'))

	# Set value
	.self$.prop[[name]] <- value

	# Notify observers
	for (obs in .self$.observers)
		if (name %in% c('scheduler.n', 'scheduler.t'))
			obs$connSchedulerFrequencyUpdated(.self)
		else if (name == 'urls')
			obs$connUrlsUpdated(.self)
})

# Set property value slot {{{1
################################################################

BiodbConnBase$methods( setPropValSlot = function(name, slot, value) {

	.self$.checkProperty(name = name, slot = slot)

	# Get current value
	curval <- .self$getPropertyValue(name)
	
	# Add/set new value
	curval[[slot]] <- value

	# Update value
	.self$setPropertyValue(name, curval)
})

# Deprecated methods {{{1
################################################################

# Get base URL {{{2
################################################################

BiodbConnBase$methods( getBaseUrl = function() {
	":\n\nReturns the base URL."

	.self$.deprecated.method("getUrl()")

	return(.self$getUrl('base.url'))
})

# Set base URL {{{2
################################################################

BiodbConnBase$methods( setBaseUrl = function(url) {
	":\n\nSets the base URL."

	.self$.deprecated.method("setUrl()")

	.self$setUrl('base.url', url)
})

# Get web sevices URL {{{2
################################################################

BiodbConnBase$methods( getWsUrl = function() {
	":\n\nReturns the web sevices URL."

	.self$.deprecated.method("getUrl()")

	return(.self$getUrl('ws.url'))
})

# Set web sevices URL {{{2
################################################################

BiodbConnBase$methods( setWsUrl = function(ws.url) {
	":\n\nSets the web sevices URL."

	.self$.deprecated.method("setUrl()")

	.self$setUrl('ws.url', ws.url)
})

# Get token {{{2
################################################################

BiodbConnBase$methods( getToken = function() {
	":\n\nReturns the access token."

	.self$.deprecated.method("getPropertyValue('token')")

	return(.self$getPropertyValue('token'))
})

# Set token {{{2
################################################################

BiodbConnBase$methods( setToken = function(token) {
	":\n\nSets the access token."

	.self$.deprecated.method("setPropertyValue('token', 'my_token_value')")

	.self$setPropertyValue('token', token)
})

# Get name {{{2
################################################################

BiodbConnBase$methods( getName = function() {
	":\n\nReturns the full database name."

	.self$.deprecated.method("getPropertyValue('name')")

	return(.self$getPropertyValue('name'))
})

# Get entry content type {{{2
################################################################

BiodbConnBase$methods( getEntryContentType = function() {
	":\n\nReturns the entry content type."

	.self$.deprecated.method("getPropertyValue('entry.content.type')")

	return(.self$getPropertyValue('entry.content.type'))
})

# Get scheduler N paramater {{{2
################################################################

BiodbConnBase$methods( getSchedulerNParam = function() {
	":\n\nReturns the N parameter for the scheduler."

	.self$.deprecated.method("getPropertyValue('scheduler.n')")

	return(.self$getPropertyValue('scheduler.n'))
})

# Set scheduler N paramater {{{2
################################################################

BiodbConnBase$methods( setSchedulerNParam = function(n) {
	":\n\nSets the N parameter for the scheduler."

	.self$.deprecated.method("setPropertyValue('scheduler.n', n)")

	.self$setPropertyValue('scheduler.n', n)
})

# Get scheduler T paramater {{{2
################################################################

BiodbConnBase$methods( getSchedulerTParam = function() {
	":\n\nReturns the T parameter for the scheduler."

	.self$.deprecated.method("getPropertyValue('scheduler.t')")

	return(.self$getPropertyValue('scheduler.t'))
})

# Set scheduler T paramater {{{2
################################################################

BiodbConnBase$methods( setSchedulerTParam = function(t) {
	":\n\nSets the T parameter for the scheduler."

	.self$.deprecated.method("setPropertyValue('scheduler.t', t)")

	.self$setPropertyValue('scheduler.t', t)
})

# Get URLs {{{2
################################################################

BiodbConnBase$methods( getUrls = function() {
	":\n\nReturns the URLs."

	.self$.deprecated.method("getPropertyValue('urls')")

	return(.self$getPropertyValue('urls'))
})

# Get a URL {{{2
################################################################

BiodbConnBase$methods( getUrl = function(name) {
	":\n\nReturns a URL."

	.self$.deprecated.method("getPropValSlot('urls', 'base.url')")

	return(.self$getPropValSlot(name='urls', slot=name))
})

# Set a URL {{{2
################################################################

BiodbConnBase$methods( setUrl = function(name, url) {
	":\n\nReturns a URL."

	.self$.deprecated.method("setPropValSlot('urls', 'base.url', 'http://my/url')")

	.self$setPropValSlot(name='urls', slot=name, value=url)

#	.self$.checkSettingOfUrl(name, url)
})

# Get XML namespace {{{2
################################################################

BiodbConnBase$methods( getXmlNs = function() {
	":\n\nReturns the XML namespace."

	.self$.deprecated.method("getPropertyValue('xml.ns')")

	return(.self$getPropertyValue('xml.ns'))
})



# Private methods {{{1
################################################################

# Terminate {{{2
################################################################

BiodbConnBase$methods( .terminate = function() {

	# Notify observers
	for (obs in .self$.observers)
		obs$connTerminating(.self)

	# Do terminate (do specific job for the connector)
	.self$.doTerminate()
})

# Do terminate {{{2
################################################################

BiodbConnBase$methods( .doTerminate = function() {
})

# Register observer {{{2
################################################################

BiodbConnBase$methods( .registerObserver = function(obs) {

	.self$.assert.inherits.from(obs, 'BiodbConnObserver')

	# Is this observer already registered?
	if (any(vapply(.self$.observers, function(x) identical(x, obs), FUN.VALUE = TRUE)))
		.self$message('caution', "Observer is already registered.")

	# Register this new observer
	else
		.self$.observers <- c(.self$.observers, obs)
})

# Unregister observer {{{2
################################################################

BiodbConnBase$methods( .unregisterObserver = function(obs) {

	.self$.assert.inherits.from(obs, 'BiodbConnObserver')

	# Search for observer
	found.obs <- vapply(.self$.observers, function(x) identical(x, obs), FUN.VALUE = TRUE)

	# Not found
	if ( ! any(found.obs))
		.self$message('caution', 'Unknown observer to unregister.')

	# Unregister observer
	else
		.self$.observers <- .self$.observers[ ! found.obs ]
})

# Check property {{{2
################################################################

BiodbConnBase$methods( .checkProperty = function(name, slot = NULL) {

	if ( ! name %in% names(.self$.prop.def))
		.self$message('error', paste0('Unknown property "', name, '" for database ', .self$getDbClass(), '.'))

	pdef <- .self$.prop.def[[name]]
	if ( ! is.null(slot) && ! 'named' %in% names(pdef))
		.self$message('error', paste0('Unauthorized use of slot "', slot, '" with unnamed property "', name, '" of database "', .self$getDbClass(), '".'))
})

# Check property value {{{2
################################################################

BiodbConnBase$methods( .checkPropertyValue = function(name, value) {

	.self$.checkProperty(name)

	pdef <- .self$.prop.def[[name]]

	# Check cardinality
	if ( ( ! 'mult' %in% names(pdef) || ! pdef$mult) && length(value) > 1)
		.self$message('error', paste0('Multiple values are forbidden for property "', name, '" of database "', .self$getDbClass(), '".'))

	# Check names
	if ('named' %in% names(pdef) && ! is.null(value) && length(value) > 0) {
		if (is.null(names(value)) || any(nchar(names(value)) == 0))
			.self$message('error', paste0('Value vector for property "', name, '"of database "', .self$getDbClass(), '" must be named. Values are: ', paste(paste(names(value), value, sep='='), collapse=', ')))
		if (any(duplicated(names(value))))
			.self$message('error', paste0('Value vector for property "', name, '"of database "', .self$getDbClass(), '" contains duplicated names.'))
	}

	# Convert value
	nms <- names(value)
	value <- as.vector(value, mode = pdef$class)
	names(value) <- nms

	# Check if value is allowed
	if (length(value) == 1) {
		if (is.na(value) && 'na.allowed' %in% names(pdef) && ! pdef$na.allowed)
			.self$message('error', paste0('NA value is not allowed for property "', name, '" of database "', .self$getDbClass(), '".'))
		if ( ! is.na(value) && 'allowed' %in% names(pdef)
	    	&& ! value %in% pdef$allowed)
			.self$message('error', paste0('Value "', value, '" is not allowed for property "', name, '" of database "', .self$getDbClass(), '".'))
	}

	return(value)
})

# Define properties {{{2
################################################################

BiodbConnBase$methods( .defineProperties = function(other, properties) {

	# Set list of property definitions
	if (is.null(other))
		.self$.prop.def <- .self$.getFullPropDefList()
	else
		.self$.prop.def <- other$.prop.def

	# Reset default values
	if ( ! is.null(properties))
		for (p in names(properties))
			.self$.prop.def[[p]]$default <- .self$.checkPropertyValue(p, properties[[p]])

	# Set property values
	if (is.null(other))
		.self$.resetPropertyValues()
	else
		.self$.prop <- other$.prop

	# Set chosen values from properties
	if ( ! is.null(properties))
		for (p in names(properties))
			.self$.prop[[p]] <- .self$.checkPropertyValue(p, properties[[p]])
})

# Reset propertyValues {{{2
################################################################

BiodbConnBase$methods( .resetPropertyValues = function() {

	.self$.prop <- list()
	for (p in names(.self$.prop.def))
		.self$setPropertyValue(p, .self$.prop.def[[p]]$default)
})

# Get full list of property definitions {{{2
################################################################

BiodbConnBase$methods( .getFullPropDefList = function() {

	# Default token
	default_token <- NA_character_
	config <- .self$getBiodb()$getConfig()
	token.key <- paste(.self$getDbClass(), 'token', sep = '.')
	if (config$isDefined(token.key, fail = FALSE))
		default_token <- config$get(token.key)

	# Define properties
	prop.def <- list(
		entry.content.encoding = list(class = 'character', default = NA_character_, na.allowed = TRUE),
		entry.content.type = list(class = 'character', default = NA_character_, allowed = c('html', 'txt', 'xml', 'csv', 'tsv', 'json', 'list'), na.allowed = FALSE, modifiable = FALSE),
		name = list(class = 'character', default = NA_character_, na.allowed = FALSE, modifiable = FALSE),
		parsing.expr = list(class = 'list', default = NULL, named = TRUE, mult = TRUE, allowed_item_types = 'character', na.allowed = FALSE, hook = 'defineParsingExpressions'),
		scheduler.n = list(class = 'integer', default = 1, na.allowed = FALSE),
		scheduler.t = list(class = 'numeric', default = 1, na.allowed = FALSE),
		token = list(class = 'character', default = default_token, na.allowed = TRUE),
		urls = list(class = 'character', default = character(), named = TRUE, mult = TRUE),
		xml.ns = list(class = 'character', default = character(), named = TRUE, mult = TRUE)
	)

	return(prop.def)
})

# Check setting of URL {{{2
################################################################

BiodbConnBase$methods( .checkSettingOfUrl = function(key, value) {
	# Accept setting by default
})

# Get class name prefix {{{2
################################################################

BiodbConnBase$methods( .getClassNamePrefix = function() {

	s <- .self$.db.class
	indices <- as.integer(gregexpr('\\.[a-z]', .self$.db.class, perl = TRUE)[[1]])
	indices <- indices + 1  # We are interested in the letter after the dot.
	indices <- c(1, indices) # Add first letter.
	for (i in indices)
		s <- paste(substring(s, 1, i - 1), toupper(substring(s, i, i)), substring(s, i + 1), sep = '')
	s <- gsub('.', '', s, fixed = TRUE) # Remove dots

	return(s)
})
