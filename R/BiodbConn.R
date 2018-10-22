# vi: fdm=marker

# Class declaration {{{1
################################################################

#' The mother abstract class of all database connectors.
#'
#' This is the super class of all connector classes. All methods defined here are thus common to all connector classes. Some connector classes inherit directly from this abstract class. Some others inherit from intermediate classes \code{\link{RemotedbConn}} and \code{\link{MassdbConn}}. As for all connector concrete classes, you won't have to create an instance of this class directly, but you will instead go through the factory class. However, if you plan to develop a new connector, you will have to call the constructor of this class. See section Fields for a list of the constructor's parameters. Concrete classes may have direct web services methods or other specific methods implemented, in which case they will be described inside the documentation of the concrete class. Please refer to the documentation of each concrete class for more information. The database direct web services methods will be named "ws.*".
#'
#' @field id            The identifier of the connector.
#'
#' @param count         If set to \code{TRUE} and no straightforward way exists to get number of entries, count the output of \code{getEntryIds()}.
#' @param entry.id      The identifiers (e.g.: accession numbers) as a \code{character vector} of the database entries.
#' @param max.results   The maximum of elements to return from the method.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{RemotedbConn}}, \code{\link{MassdbConn}}.
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
BiodbConn <- methods::setRefClass("BiodbConn", contains = "ChildObject", fields = list(.id = "character", .dbinfo = "any"))

# Constructor {{{1
################################################################

BiodbConn$methods( initialize = function(id = NA_character_, dbinfo = NULL, ...) {

	callSuper(...)
	.self$.abstract.class('BiodbConn')

	.self$.assert.is(dbinfo, "BiodbDbInfo")
	.self$.assert.is(id, "character")
	.id <<- id
	.dbinfo <<- dbinfo
    # TODO Another scheme would be to copy the db.info field values into fields of the connector instance.
	# TODO Yet another solution would be to get the BiodbDbInfo instance directly by contacting the BiodbDbsInfo instance and using a database type given by parameters by the factory.
})

# Get id {{{1
################################################################

BiodbConn$methods( getId = function() {
	":\n\nGet the identifier of this connector."

	return(.self$.id)
})

# Get database info {{{1
################################################################

BiodbConn$methods( getDbInfo = function() {
	":\n\nGet the database information associated with this connector."

	return(.self$.dbinfo)
})

# Get base URL {{{1
################################################################

BiodbConn$methods( getBaseUrl = function() {
	":\n\nGet the base URL of this connector."

	return(.self$getDbInfo()$getBaseUrl()) 
})

# Get web service URL {{{1
################################################################

BiodbConn$methods( getWsUrl = function() {
	":\n\nGet the web service URL of this connector."

	return(.self$getDbInfo()$getWsUrl()) 
})

# Get entry content type {{{1
################################################################

BiodbConn$methods( getEntryContentType = function() {
	":\n\nGet the base URL of the content type."

	return(.self$getDbInfo()$getEntryContentType()) 
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

# Show {{{1
################################################################

BiodbConn$methods( show = function() {
	cat("Biodb ", .self$getDbInfo()$getName(), " connector instance, using URL \"", .self$getBaseUrl(), "\".\n", sep = '')
})

# Check database {{{1
################################################################

BiodbConn$methods( checkDb = function() {
	":\n\nCheck that the database is correct by trying to load all entries."

	# Get IDs
	ids <- .self$getEntryIds()

	# Get entries
	entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), ids)
})

# PRIVATE METHODS {{{1
################################################################

# Set base URL {{{2
################################################################

BiodbConn$methods( .setBaseUrl = function(base.url) {

	if (is.null(base.url) || any(is.na(base.url)))
		.self$message('error', "Cannot set a NULL or NA URL to a database connector.")

	.self$getDbInfo()$setBaseUrl(base.url)
})
