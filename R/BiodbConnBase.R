# vi: fdm=marker

# Class declaration {{{1
################################################################

#' Base class of \code{BiodbConn} for encapsulating all needed information for database access.
#'
#' This is the base class for \code{BiodbConn} and \code{BiodbDbInfo}. The constructor is not meant to be used, but for development purposes the constructor's parameters are nevertheless described in the Fields section.
#'
#' @field db.class      The class of the database (\code{"massbank", "hmdb.metabolies", ...}).
#' @field base.url      The main URL of the database.
#' @field ws.url        The web services URL of the database.
#' @field xml.ns        The XML namespace used by the database.
#' @field token         An access token for the database.
#' @field scheduler.n   The N parameter for the scheduler. The N paramter is the number of request allowed for each bit of time (defined by the T parameter).
#' @field scheduler.t   The T parameter for the scheduler. The bit of time, in seconds, during which a maximum of N (see scheduler.n) requests is allowed.
#'
#' @param base.url  A base URL for the database.
#' @param n         The scheduler "number of requests parameter.
#' @param t         The scheduler "time" parameter, in seconds.
#' @param token     An access token for the database.
#' @param ws.url    A web service URL for the database.
#' @param xml.ns    The XML namespace used by the database.
#'
#' @seealso \code{\link{BiodbDbsInfo}}, \code{\link{BiodbConn}}.
#'
#' @import methods
#' @include ChildObject.R
#' @export BiodbConnBase
#' @exportClass BiodbConnBase
BiodbConnBase <- methods::setRefClass("BiodbConnBase", contains =  "ChildObject", fields = list( .db.class = "character", .base.url = "character", .ws.url = 'character', .token = "character", .scheduler.n = 'integer', .scheduler.t = 'integer', .entry.content.type = 'character', .xml.ns = 'character', .name = 'character', .observers = 'list'))

# Constructor {{{1
################################################################

BiodbConnBase$methods( initialize = function(other = NULL, db.class = NULL, base.url = NA_character_, ws.url = NA_character_, scheduler.n = 1, scheduler.t = 1, entry.content.type = NA_character_, xml.ns = NA_character_, name = NA_character_, token = NA_character_, ...) {

	callSuper(...)
	.self$.abstract.class('BiodbConnBase')

	# Take parameter values from other object instance
	if ( ! is.null(other)) {
		.self$.assert.inherits.from(other, "BiodbConnBase")
		for (param in c('db.class', 'base.url', 'ws.url', 'scheduler.n', 'scheduler.t', 'entry.content.type', 'xml.ns', 'name'))
			if (is.null(get(param)) || is.na(get(param)))
				assign(param, other[[paste0('.', param)]])
	}

	# Set database class
	.self$.assert.not.null(db.class)
	.self$.assert.not.na(db.class)
	.self$.assert.is(db.class, 'character')
	.db.class <<- db.class

	# Set entry.content type
	.self$.assert.not.null(entry.content.type)
	.self$.assert.not.na(entry.content.type)
	.self$.assert.is(entry.content.type, 'character')
	.self$.assert.in(entry.content.type, c('html', 'txt', 'xml', 'csv', 'tsv', 'json'))
	.entry.content.type <<- entry.content.type

	# URLs
	.self$.assert.is(base.url, 'character')
	.self$.assert.is(ws.url, 'character')
	.base.url <<- base.url
	.ws.url <<- ws.url

	# Other parameters
	.self$.assert.is(name, 'character')
	.self$.assert.is(xml.ns, 'character')
	.self$.assert.is(token, 'character')
	.self$.assert.is(scheduler.n, c('integer', 'numeric'))
	.self$.assert.is(scheduler.t, c('integer', 'numeric'))
	.name <<- name
	.xml.ns <<- xml.ns
	if (is.na(token)) {
		config <- .self$getBiodb()$getConfig()
		token.key <- paste(db.class, 'token', sep = '.')
		.token <<- if (config$isDefined(token.key, fail = FALSE)) config$get(token.key) else NA_character_
	}
	.scheduler.n <<- as.integer(scheduler.n)
	.scheduler.t <<- as.integer(scheduler.t)
	.observers <<- list()
})

# Get name {{{1
################################################################

BiodbConnBase$methods( getName = function() {
	":\n\nReturns the full database name."

	return(.self$.name)
})

# Get entry content type {{{1
################################################################

BiodbConnBase$methods( getEntryContentType = function() {
	":\n\nReturns the entry content type."

	return(.self$.entry.content.type)
})

# Get database class {{{1
################################################################

BiodbConnBase$methods( getDbClass = function() {
	return(.self$.db.class)
})

# Get connection class name {{{1
################################################################

BiodbConnBase$methods( getConnClassName = function() {
	":\n\nReturns the name of the associated connection class."

    # Get connection class name
    s <- .self$.db.class
	indices <- as.integer(gregexpr('\\.[a-z]', .self$.db.class, perl = TRUE)[[1]])
    indices <- indices + 1  # We are interested in the letter after the dot.
    indices <- c(1, indices) # Add first letter.
	for (i in indices)
		s <- paste(substring(s, 1, i - 1), toupper(substring(s, i, i)), substring(s, i + 1), sep = '')
    s <- gsub('.', '', s, fixed = TRUE) # Remove dots
	conn.class.name <- paste(s, 'Conn', sep = '')

	return(conn.class.name)
})

# Get connection class {{{1
################################################################

BiodbConnBase$methods( getConnClass = function() {
	":\n\nReturns the associated connection class."

	return(get(.self$getConnClassName()))
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

	urls <- character()
	if ( ! is.null(.self$.base.url) && ! is.na(.self$.base.url))
		urls <- c(urls, .self$.base.url)
	if ( ! is.null(.self$.ws.url) && ! is.na(.self$.ws.url))
		urls <- c(urls, .self$.ws.url)

	return(urls)
})

# Get base URL {{{1
################################################################

BiodbConnBase$methods( getBaseUrl = function() {
	":\n\nReturns the base URL."

	return(.self$.base.url)
})

# Set base URL {{{1
################################################################

BiodbConnBase$methods( setBaseUrl = function(base.url) {
	":\n\nSets the base URL."

	.base.url <<- base.url

	# Notify observers
	for (obs in .self$.observers)
		obs$connUrlsUpdated(.self)
})
# Get web sevices URL {{{1
################################################################

BiodbConnBase$methods( getWsUrl = function() {
	":\n\nReturns the web sevices URL."

	return(.self$.ws.url)
})

# Set web sevices URL {{{1
################################################################

BiodbConnBase$methods( setWsUrl = function(ws.url) {
	":\n\nSets the web sevices URL."

	.ws.url <<- ws.url

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

# Set XML namespace {{{1
################################################################

BiodbConnBase$methods( setXmlNs = function(xml.ns) {
	":\n\nSets the XML namespace."

	.xml.ns <<- xml.ns
})

# Get token {{{1
################################################################

BiodbConnBase$methods( getToken = function() {
	":\n\nReturns the access token."

	return(.self$.token)
})

# Set token {{{1
################################################################

BiodbConnBase$methods( setToken = function(token) {
	":\n\nSets the access token."

	.token <<- token
})

# Get scheduler N paramater {{{1
################################################################

BiodbConnBase$methods( getSchedulerNParam = function() {
	":\n\nReturns the N parameter for the scheduler."

	return(.self$.scheduler.n)
})

# Set scheduler N paramater {{{1
################################################################

BiodbConnBase$methods( setSchedulerNParam = function(n) {
	":\n\nSets the N parameter for the scheduler."

	.scheduler.n <<- as.integer(n)

	# Notify observers
	for (obs in .self$.observers)
		obs$connSchedulerFrequencyUpdated(.self)
})

# Get scheduler T paramater {{{1
################################################################

BiodbConnBase$methods( getSchedulerTParam = function() {
	":\n\nReturns the T parameter for the scheduler."

	return(.self$.scheduler.t)
})

# Set scheduler T paramater {{{1
################################################################

BiodbConnBase$methods( setSchedulerTParam = function(t) {
	":\n\nSets the T parameter for the scheduler."

	.scheduler.t <<- as.integer(t)

	# Notify observers
	for (obs in .self$.observers)
		obs$connSchedulerFrequencyUpdated(.self)
})

# Private methods {{{1
################################################################

# Terminate {{{2
################################################################

BiodbConnBase$methods( .terminate = function() {

	# Notify observers
	for (obs in .self$.observers)
		obs$connTerminating(.self)
})

# Register observer {{{2
################################################################

BiodbConnBase$methods( .registerObserver = function(obs) {

	.self$.assert.is(obs, 'BiodbConnObserver')

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

	.self$.assert.is(obs, 'BiodbConnObserver')

	# Search for observer
	found.obs <- vapply(.self$.observers, function(x) identical(x, obs), FUN.VALUE = TRUE)

	# Not found
	if ( ! any(found.obs))
		.self$message('caution', 'Unknown observer to unregister.')

	# Unregister observer
	else
		.observers <<- .self$.observers[ ! found.obs ]
})
