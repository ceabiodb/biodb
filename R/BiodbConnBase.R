# vi: fdm=marker

# Class declaration {{{1
################################################################

#' Base class of \code{BiodbConn} for encapsulating all needed information for database access.
#'
#' This is the base class for \code{BiodbConn} and \code{BiodbDbInfo}. The constructor is not meant to be used, but for development purposes the constructor's parameters are nevertheless described in the Fields section.
#'
#' @field db.class      The class of the database (\code{"massbank", "hmdb.metabolies", ...}).
#' @field urls          The URLs of the database.
#' @field xml.ns        The XML namespace used by the database.
#' @field prop.def      Definitions of the properties.
#' @field prop          Values of the properties.
#'
#' @param url       A URL for the database.
#' @param xml.ns    The XML namespace used by the database.
#'
#' @seealso \code{\link{BiodbDbsInfo}}, \code{\link{BiodbConn}}.
#'
#' @import methods
#' @include BiodbChildObject.R
#' @export BiodbConnBase
#' @exportClass BiodbConnBase
BiodbConnBase <- methods::setRefClass("BiodbConnBase", contains =  "BiodbChildObject", fields = list( .db.class = "character", .urls = "character", .xml.ns = 'character', .observers = 'list', .prop.def = 'list', .prop = 'list'))

# Constructor {{{1
################################################################

BiodbConnBase$methods( initialize = function(other = NULL, db.class = NULL, urls = NULL, xml.ns = NULL, properties = NULL, ...) {

	callSuper(...)
	.self$.abstract.class('BiodbConnBase')

	# Take parameter values from other object instance
	if ( ! is.null(other)) {
		.self$.assert.inherits.from(other, "BiodbConnBase")
		for (param in c('db.class', 'urls', 'xml.ns'))
			if (is.null(get(param)) || is.na(get(param)))
				assign(param, other[[paste0('.', param)]])
	}

	# Set database class
	.self$.assert.not.null(db.class)
	.self$.assert.not.na(db.class)
	.self$.assert.is(db.class, 'character')
	.db.class <<- db.class

	# URLs
	.self$.assert.is(urls, 'character')
	.urls <<- urls

	# Other parameters
	.self$.assert.is(xml.ns, 'character')
	.xml.ns <<- if (is.null(xml.ns)) character() else xml.ns
	.observers <<- list()

	# Set properties
	if (is.null(other))
		.self$.defineProperties(properties)
	else {
		.prop.def <<- other$.prop.def
		.prop <<- other$.prop
	}
})

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

# Get URLs {{{1
################################################################

BiodbConnBase$methods( getUrls = function() {
	":\n\nReturns the URLs."

	return(.self$.urls)
})

# Get a URL {{{1
################################################################

BiodbConnBase$methods( getUrl = function(name) {
	":\n\nReturns a URL."

	url <- NA_character_

	if (name %in% names(.self$.urls))
		url <- .self$.urls[[name]]

	return(url)
})

# Set a URL {{{1
################################################################

BiodbConnBase$methods( setUrl = function(name, url) {
	":\n\nReturns a URL."

	.self$.checkSettingOfUrl(name, url)

	.self$.urls[[name]] <- url

	# Notify observers
	for (obs in .self$.observers)
		obs$connUrlsUpdated(.self)
})

# Get XML namespace {{{1
################################################################

BiodbConnBase$methods( getXmlNs = function() {
	":\n\nReturns the XML namespace."

	return(.self$.xml.ns)
})

# Get property value {{{1
################################################################

BiodbConnBase$methods( getPropertyValue = function(name) {

	.self$.checkProperty(name)

	if (name %in% names(.self$.prop))
		value <- .self$.prop[[name]]
	else
		value <- .self$.prop.def[[name]]$default

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
	if (name %in% c('scheduler.n', 'scheduler.t'))
		for (obs in .self$.observers)
			obs$connSchedulerFrequencyUpdated(.self)
})

# Get property value slot {{{1
################################################################

BiodbConnBase$methods( getPropValSlot = function(name, slot) {

	value <- .self$getPropertyValue(name)
	.self$.checkProperty(name = name, slot = slot)

	if ( ! slot %in% names(value))
		.self$message('error', 'Slot "', slot, '" is not defined in named property "', name, '" of database "', .self$getDbClass(), '".')
	value <- value[[slot]]

	return(value)
})

# Set property value slot {{{1
################################################################

BiodbConnBase$methods( setPropValSlot = function(name, slot, value) {

	.self$.checkProperty(name = name, slot = slot)

	# TODO Set value with slot
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

# Get name {{{1
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
		.observers <<- c(.self$.observers, obs)
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
		.observers <<- .self$.observers[ ! found.obs ]
})

# Check property {{{2
################################################################

BiodbConnBase$methods( .checkProperty = function(name, slot = NULL) {

	if ( ! name %in% names(.self$.prop.def))
		.self$message('error', paste0('Unknown property "', name, '" for database ', .self$getDbClass(), '.'))

	pdef <- .self$.prop.def[[name]]
	if ( ! 'named' %in% names(pdef))
		.self$message('error', 'Unauthorized use of slot "', slot, '", property "', name, '" of database "', .self$getDbClass(), '" is unnamed.')
})

# Check property value {{{2
################################################################

BiodbConnBase$methods( .checkPropertyValue = function(name, value) {

	.self$.checkProperty(name)

	pdef <- .self$.prop.def[[name]]

	# Check cardinality
	if ( ! 'mult' %in% names(pdef) || ! pdef$mult && length(value) > 1)
		.self$message('error', paste0('Multiple values are forbidden for property "', name, '" of database "', self$getDbClass(), '".'))

	# Check names
	if ('named' %in% names(pdef)) {
		if (is.null(names(value)) || any(nchar(names(value)) == 0))
			.self$message('error', paste0('Value vector for property "', name, '"of database "', self$getDbClass(), '" must be named.'))
		if (any(duplicated(names(value))))
			.self$message('error', paste0('Value vector for property "', name, '"of database "', self$getDbClass(), '" contains duplicated names.'))
	}

	# Convert value
	value <- as.vector(value, mode = pdef$class)

	# Check if value is allowed
	if (is.na(value) && 'na.allowed' %in% names(pdef) && ! pdef$na.allowed)
		.self$message('error', paste0('NA value is not allowed for property "', name, '" of database "', self$getDbClass(), '".'))
	if ( ! is.na(value) && 'allowed' %in% names(pdef)
	    && ! value %in% pdef$allowed)
		.self$message('error', paste0('Value "', value, '" is not allowed for property "', name, '" of database "', self$getDbClass(), '".'))

	return(value)
})

# Define properties {{{2
################################################################

BiodbConnBase$methods( .defineProperties = function(properties) {

	# Set list of property definitions
	.prop.def <<- .self$.getFullPropDefList()

	# Select subset of properties
	if ( ! is.null(properties)) {

		# Reset default values
		for (p in names(properties))
			.self$.prop.def[[p]]$default <- .self$.checkPropertyValue(p, properties[[p]])
	}

	# Reset property values
	.self$.resetPropertyValues()
})

# Reset propertyValues {{{2
################################################################

BiodbConnBase$methods( .resetPropertyValues = function() {

	.prop <<- list()
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
		scheduler.n = list(class = 'integer', default = 1, na.allowed = FALSE),
		scheduler.t = list(class = 'numeric', default = 1, na.allowed = FALSE),
		urls = list(class = 'character', default = character(), named = TRUE, mult = TRUE),
		token = list(class = 'character', default = default_token, na.allowed = TRUE)
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
