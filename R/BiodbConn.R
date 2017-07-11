# vi: fdm=marker

# Class declaration {{{1
################################################################

#' The mother abstract class of all database connectors.
#'
#' This is the super class of all connector classes. All methods defined here are thus common to all connector classes. Some connector classes inherit directly from this abstract class. Some others inherit from intermediate classes \code{\link{RemotedbConn}} and \code{\link{MassdbConn}}. As for all connector concrete classes, you won't have to create an instance of this class directly, but you will instead go through the factory class. However, if you plan to develop a new connector, you will have to call the constructor of this class. See section Fields for a list of the constructor's parameters.
#'
#' @field dbid          The identifier of the connector.
#' @field content.type  The type of content handled by the connector for entries: XML, TXT, etc.
#' @field base.url      The base URL of the database. Default values are defined inside \code{\link{BiodbDbsInfo}}.
#'
#' @param entry.id      The identifiers (e.g.: accession numbers) as a \code{character vector} of the database entries.
#' @param max.results   The maximum of elements to return from the method.
#' @param count         If set to \code{TRUE} and no straightforward way exists to get number of entries, count the output of \code{getEntryIds()}.
#'
#' @seealso \code{\link\{BiodbFactory}}, \code{\link{RemotedbConn}}, \code{\link{MassdbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('chebi')
#'
#' # Get 10 identifiers from the database:
#' ids <- conn$getEntryIds(10)
#'
#' # Get number of entries contained in the database:
#' n <- conn$getNbEntries()
#'
#' @import methods
#' @include ChildObject.R
#' @export BiodbConn
#' @exportClass BiodbConn
BiodbConn <- methods::setRefClass("BiodbConn", contains = "ChildObject", fields = list(.dbid = "character", .content.type = "character", .base.url = "character"))

# Constructor {{{1
################################################################

BiodbConn$methods( initialize = function(dbid = NA_character_, content.type = NA_character_, base.url = NA_character_, ...) {

	callSuper(...)

	.dbid <<- dbid

	# Set content type
	if (is.null(content.type) || is.na(content.type))
		.self$message(MSG.ERROR, "Content type not defined.")
	if ( ! content.type %in% BIODB.CONTENT.TYPES)
		.self$message(MSG.ERROR, paste("Unknown content type \"", content.type, "\"."))
	.content.type <<- content.type

	# Set base URL
	if (is.na(base.url))
		base.url <- .self$getBiodb()$getDbsInfo()$get(.self$getId())$getBaseUrl()
	.self$.setBaseUrl(base.url)
})

# Get id {{{1
################################################################

BiodbConn$methods( getId = function() {
	":\n\nGet the identifier of this connector."

	return(.self$.dbid)
})

# Get base URL {{{1
################################################################

BiodbConn$methods( getBaseUrl = function() {
	":\n\nGet the base URL of this connector."

	return(.self$.base.url)
})

# Get entry content type {{{1
################################################################

BiodbConn$methods( getEntryContentType = function() {
	":\n\nGet the base URL of the content type."

	return(.self$.content.type) 
})

# Get entry content {{{1
################################################################

BiodbConn$methods( getEntryContent = function(entry.id) {
	":\n\nGet the content of an entry."

	.self$.abstract.method()
})


# Get entry ids {{{1
################################################################

BiodbConn$methods( getEntryIds = function(max.results = NA_integer_) {
	":\n\nGet entry identifiers from the database."

	.self$.abstract.method()
})

# Get nb entries {{{1
################################################################

BiodbConn$methods( getNbEntries = function(count = FALSE) {
	":\n\nGet the number of entries contained in this database."

	n <- NA_integer_

	if (count) {
		ids <- .self$getEntryIds()
		if ( ! is.null(ids))
			n <- length(ids)
	}

	return(n)
})

# PRIVATE METHODS {{{1
################################################################

# Set base URL {{{2
################################################################

BiodbConn$methods( .setBaseUrl = function(base.url) {

	if (is.null(base.url) || any(is.na(base.url)))
		.self$message(MSG.ERROR, "Cannot set a NULL or NA URL to a database connector.")

	.base.url <<- base.url
})
