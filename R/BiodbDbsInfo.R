# vi: fdm=marker

# Class declaration {{{1
################################################################

#' A class for describing the available databases.
#'
#' The unique instance of this class is handle by the \code{\link{Biodb}} class and accessed through the \code{getDbsInfo()} method.
#'
#' @seealso \code{\link{Biodb}}, \code{\link{BiodbDbInfo}}.
#'
#' @param db.id A database ID, as a character string.
#'
#' @examples
#' # Getting the base URL of a database:
#' mybiodb <- biodb::Biodb()
#' chebi.base.url <- mybiodb$getDbsInfo()$get('chebi')$getPropValSlot('urls', 'base.url')
#'
#' # Setting a token:
#' mybiodb$getDbsInfo()$get('chemspider')$setToken('my.chemspider.token')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @include BiodbChildObject.R
#' @include BiodbDbInfo.R
#' @export BiodbDbsInfo
#' @exportClass BiodbDbsInfo
BiodbDbsInfo <- methods::setRefClass("BiodbDbsInfo", contains =  "BiodbChildObject", fields = list( .dbs = "list"))

# Constructor {{{1
################################################################

BiodbDbsInfo$methods( initialize = function(...) {

	callSuper(...)

	.dbs <<- list()

	.self$.initDbsInfo()
})

# Get list of database IDs {{{1
################################################################

BiodbDbsInfo$methods( getIds = function() {
	":\n\nReturns a character vector containing all the IDs of the defined databases."

	return(names(.self$.dbs))
})

# Is defined {{{1
################################################################

BiodbDbsInfo$methods( isDefined = function(db.id) {
	":\n\nReturns TRUE if the specified id corresponds to a defined database."

	return(db.id %in% names(.self$.dbs))
})

# Check is defined {{{1
################################################################

BiodbDbsInfo$methods( checkIsDefined = function(db.id) {
	":\n\nThrows an error if the specified id does not correspond to a defined database."

	if ( ! .self$isDefined(db.id))
		.self$message('error', paste("Database \"", db.id, "\" is not defined.", sep = ''))
})

# Get {{{1
################################################################

BiodbDbsInfo$methods( get = function(db.id) {
	":\n\nReturns the BiodbDbInfo instance corresponding to the specified database ID."

	.self$checkIsDefined(db.id)
	db <- .self$.dbs[[db.id]]
	return(db)
})

# Get all {{{1
################################################################

BiodbDbsInfo$methods( getAll = function() {
	":\n\nReturns a list of all BiodbDbInfo instances."

	return(unname(.self$.dbs))
})

# Show {{{1
################################################################

BiodbDbsInfo$methods( show = function() {
	cat("Biodb databases information instance.\n")
})

# Load info file {{{1
################################################################

BiodbDbsInfo$methods( loadInfoFile = function(file) {
	'Load databases information file, and defines the new databases.
	The parameter file must point to a valid JSON file.'

	dbi <- jsonlite::fromJSON(file, simplifyDataFrame = FALSE)

	# Loop on all db info
	for (db in names(dbi))
		.self$.define(db, properties = dbi[[db]])
})

# Private methods {{{1
################################################################

# Initialize databases information {{{2
################################################################

BiodbDbsInfo$methods( .initDbsInfo = function() {

	# Load JSON databases information file
	dbf <- .self$getBiodb()$getConfig()$get('dbinfo.file')
	.self$loadInfoFile(dbf)
})

# Define {{{2
################################################################

BiodbDbsInfo$methods( .define = function(id, ...) {

	# Is this database already defined?
	if (id %in% names(.self$.dbs))
		.self$message('error', paste("Database \"", id, "\" has already been defined.", sep = ''))

	# Define new field
	.self$.dbs[[id]] <- BiodbDbInfo$new(parent = .self, db.class = id, ...)
})
