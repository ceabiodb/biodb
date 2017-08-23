# vi: fdm=marker

# Class declaration {{{1
################################################################

#' A class for describing the characteristics of a database.
#'
#' This class is used by \code{\link{BiodbDbsInfo}} for storing database characteristics, and returning them through the \code{get()} method. The constructor is not meant to be used, but for development purposes the constructor's parameters are nevertheless described in the Fields section.
#'
#' @field id        The identifier of the database.
#' @field base.url  The main URL of the database.
#' @field token     An access token for the database.
#'
#' @seealso \code{\link{BiodbDbsInfo}}.
#'
#' @param base.url  A base URL for the database.
#' @param token     An access token for the database.
#'
#' @import methods
#' @include ChildObject.R
#' @export BiodbDbInfo
#' @exportClass BiodbDbInfo
BiodbDbInfo <- methods::setRefClass("BiodbDbInfo", contains =  "ChildObject", fields = list( .id = "character", .base.url = "character", .token = "character"))

# Constructor {{{1
################################################################

BiodbDbInfo$methods( initialize = function(id, base.url = NA_character_, token = NA_character_, ...) {

	callSuper(...)

	# Set id
	if ( is.null(id) || is.na(id) || nchar(id) == '')
		.self$message('error', "You cannot set an empty id for a database. The ID was empty (either NULL or NA or empty string).")
	.id <<- id

	.base.url <<- base.url
	.token <<- token
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

# Get base url {{{1
################################################################

BiodbDbInfo$methods( getBaseUrl = function() {
	":\n\nReturns the base URL."

	return(.self$.base.url)
})

# Set base url {{{1
################################################################

BiodbDbInfo$methods( setBaseUrl = function(base.url) {
	":\n\nSets the base URL."

	.base.url <<- base.url
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
