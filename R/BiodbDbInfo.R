# vi: fdm=marker

# Class declaration {{{1
################################################################

#' A class for describing the characteristics of a database.
#'
#' This class is used by \code{\link{BiodbDbsInfo}} for storing database characteristics, and returning them through the \code{get()} method. The constructor is not meant to be used, but for development purposes the constructor's parameters are nevertheless described in the Fields section.
#'
#' @field id            The identifier of the database.
#' @field base.url      The main URL of the database.
#' @field ws.url        The web services URL of the database.
#' @field xml.ns        The XML namespace used by the database.
#' @field token         An access token for the database.
#' @field scheduler.n   The N parameter for the scheduler. The N paramter is the number of request allowed for each bit of time (defined by the T parameter).
#' @field scheduler.t   The T parameter for the scheduler. The bit of time, in seconds, during which a maximum of N (see scheduler.n) requests is allowed.
#'
#' @seealso \code{\link{BiodbDbsInfo}}.
#'
#' @param base.url  A base URL for the database.
#' @param ws.url    A web service URL for the database.
#' @field xml.ns    The XML namespace used by the database.
#' @param token     An access token for the database.
#'
#' @import methods
#' @include ChildObject.R
#' @export BiodbDbInfo
#' @exportClass BiodbDbInfo
BiodbDbInfo <- methods::setRefClass("BiodbDbInfo", contains =  "ChildObject", fields = list( .id = "character", .base.url = "character", .ws.url = 'character', .token = "character", .scheduler.n = 'integer', .scheduler.t = 'integer', .entry.content.type = 'character', .xml.ns = 'character'))

# Constructor {{{1
################################################################

BiodbDbInfo$methods( initialize = function(id, base.url = NA_character_, ws.url = NA_character_, scheduler.n = 1, scheduler.t = 1, entry.content.type = NA_character_, xml.ns = NA_character_, ...) {

	callSuper(...)
	config <- .self$getBiodb()$getConfig()

	# Set id
	if ( is.null(id) || is.na(id) || nchar(id) == '')
		.self$message('error', "You cannot set an empty id for a database. The ID was empty (either NULL or NA or empty string).")
	.id <<- id

	# Set entry.content type
	if (is.null(entry.content.type) || is.na(entry.content.type))
		.self$message('error', "Content type not defined.")
	if ( ! entry.content.type %in% c('html', 'txt', 'xml', 'csv', 'tsv', 'json'))
		.self$message('error', paste("Unknown entry.content type \"", entry.content.type, "\"."))
	.entry.content.type <<- entry.content.type

	.base.url <<- base.url
	.ws.url <<- ws.url
	.xml.ns <<- xml.ns
	token.key <- paste(id, 'token', sep = '.')
	.token <<- if (config$isDefined(token.key, fail = FALSE)) config$get(token.key) else NA_character_
	.scheduler.n <<- as.integer(scheduler.n)
	.scheduler.t <<- as.integer(scheduler.t)
})

# Get entry content type {{{1
################################################################

BiodbDbInfo$methods( getEntryContentType = function() {
	":\n\nReturns the entry content type."

	return(.self$.entry.content.type)
})

# Get connection class name {{{1
################################################################

BiodbDbInfo$methods( getConnClassName = function() {
	":\n\nReturns the name of the associated connection class."

    # Get connection class name
    s <- .self$.id
	indices <- as.integer(gregexpr('\\.[a-z]', .self$.id, perl = TRUE)[[1]])
    indices <- indices + 1  # We are interested in the letter after the dot.
    indices <- c(1, indices) # Add first letter.
	for (i in indices)
		s <- paste(substring(s, 1, i - 1), toupper(substring(s, i, i)), substring(s, i + 1), sep = '')
    s <- gsub('.', '', s, fixed = TRUE) # Remove dots
	conn.class.id <- paste(s, 'Conn', sep = '')

	return(conn.class.id)
})

# Get connection class {{{1
################################################################

BiodbDbInfo$methods( getConnClass = function() {
	":\n\nReturns the associated connection class."

	return(get(.self$getConnClassName()))
})

# Get entry class name {{{1
################################################################

BiodbDbInfo$methods( getEntryClassName = function() {
	":\n\nReturns the name of the associated entry class."

    # Get entry class name
	s <- .self$.id
	indices <- as.integer(gregexpr('\\.[a-z]', .self$.id, perl = TRUE)[[1]])
	indices <- indices + 1  # We are interested in the letter after the dot.
	indices <- c(1, indices) # Add first letter.
	for (i in indices)
		s <- paste(substring(s, 1, i - 1), toupper(substring(s, i, i)), substring(s, i + 1), sep = '')
	s <- gsub('.', '', s, fixed = TRUE) # Remove dots
	entry.class.id <- paste(s, 'Entry', sep = '')

	return(entry.class.id)
})

# Get entry class {{{1
################################################################

BiodbDbInfo$methods( getEntryClass = function() {
	":\n\nReturns the associated entry class."

	return(get(.self$getEntryClassName()))
})

# Get entry ID field {{{1
################################################################

BiodbDbInfo$methods( getEntryIdField = function() {
	":\n\nReturn the name of the corresponding database ID field in entries."
	
	return(paste(.self$.id, 'id', sep = '.'))
})

# Get base URL {{{1
################################################################

BiodbDbInfo$methods( getBaseUrl = function() {
	":\n\nReturns the base URL."

	return(.self$.base.url)
})

# Set base URL {{{1
################################################################

BiodbDbInfo$methods( setBaseUrl = function(base.url) {
	":\n\nSets the base URL."

	.base.url <<- base.url
})
# Get web sevices URL {{{1
################################################################

BiodbDbInfo$methods( getWsUrl = function() {
	":\n\nReturns the web sevices URL."

	return(.self$.ws.url)
})

# Set web sevices URL {{{1
################################################################

BiodbDbInfo$methods( setWsUrl = function(ws.url) {
	":\n\nSets the web sevices URL."

	.ws.url <<- ws.url
})

# Get XML namespace {{{1
################################################################

BiodbDbInfo$methods( getXmlNs = function() {
	":\n\nReturns the XML namespace."

	return(.self$.xml.ns)
})

# Set XML namespace {{{1
################################################################

BiodbDbInfo$methods( setXmlNs = function(xml.ns) {
	":\n\nSets the XML namespace."

	.xml.ns <<- xml.ns
})

# Get token {{{1
################################################################

BiodbDbInfo$methods( getToken = function() {
	":\n\nReturns the access token."

	return(.self$.token)
})

# Set token {{{1
################################################################

BiodbDbInfo$methods( setToken = function(token) {
	":\n\nSets the access token."

	.token <<- token
})

# Get scheduler N paramater {{{1
################################################################

BiodbDbInfo$methods( getSchedulerNParam = function() {
	":\n\nReturns the N parameter for the scheduler."

	return(.self$.scheduler.n)
})

# Set scheduler N paramater {{{1
################################################################

BiodbDbInfo$methods( setSchedulerNParam = function(n) {
	":\n\nSets the N parameter for the scheduler."

	.scheduler.n <<- as.integer(n)
})

# Get scheduler T paramater {{{1
################################################################

BiodbDbInfo$methods( getSchedulerTParam = function() {
	":\n\nReturns the T parameter for the scheduler."

	return(.self$.scheduler.t)
})

# Set scheduler T paramater {{{1
################################################################

BiodbDbInfo$methods( setSchedulerTParam = function(t) {
	":\n\nSets the T parameter for the scheduler."

	.scheduler.t <<- as.integer(t)
})
