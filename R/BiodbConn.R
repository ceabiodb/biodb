# vi: fdm=marker

# Class declaration {{{1
################################################################

#' The mother abstract class of all database connectors.
#'
#' This is the super class of all connector classes. All methods defined here are thus common to all connector classes. Some connector classes inherit directly from this abstract class. Some others inherit from intermediate classes \code{\link{BiodbRemotedbConn}} and \code{\link{BiodbMassdbConn}}. As for all connector concrete classes, you won't have to create an instance of this class directly, but you will instead go through the factory class. However, if you plan to develop a new connector, you will have to call the constructor of this class. See section Fields for a list of the constructor's parameters. Concrete classes may have direct web services methods or other specific methods implemented, in which case they will be described inside the documentation of the concrete class. Please refer to the documentation of each concrete class for more information. The database direct web services methods will be named "ws.*".
#'
#' @field id            The identifier of the connector.
#' @field cache.id      The identifier used in the disk cache.
#'
#' @param count         If set to \code{TRUE} and no straightforward way exists to get number of entries, count the output of \code{getEntryIds()}.
#' @param entry.id      The identifiers (e.g.: accession numbers) as a \code{character vector} of the database entries.
#' @param max.results   The maximum of elements to return from the method.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{BiodbRemotedbConn}}, \code{\link{BiodbMassdbConn}}.
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
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @include BiodbConnBase.R
#' @export BiodbConn
#' @exportClass BiodbConn
BiodbConn <- methods::setRefClass("BiodbConn", contains = "BiodbConnBase", fields = list(.id = "character", .entries = "list", .cache.id = 'character'))

# Constructor {{{1
################################################################

BiodbConn$methods( initialize = function(id = NA_character_, cache.id = NA_character_, ...) {

	callSuper(...)
	.self$.abstract.class('BiodbConn')

	.self$.assert.is(id, "character")
	.id <<- id
	.cache.id <<- if (is.null(cache.id)) NA_character_ else cache.id
	.entries <<- list()
})

# Get id {{{1
################################################################

BiodbConn$methods( getId = function() {
	":\n\nGet the identifier of this connector."

	return(.self$.id)
})

# Get entry {{{1
################################################################

BiodbConn$methods( getEntry = function(id, drop = TRUE) {
	":\n\nReturn the entry corresponding to this ID. You can pass a vector of IDs, and you will get a list of entries."

	return(.self$getBiodb()$getFactory()$getEntry(.self$getId(), id = id, drop = drop))
})

# Get entry content {{{1
################################################################

BiodbConn$methods( getEntryContent = function(entry.id) {
	":\n\nGet the content of an entry."

	.self$.abstract.method()
})


# Get entry ids {{{1
################################################################

BiodbConn$methods( getEntryIds = function(max.results = NA_integer_, ...) {
	":\n\nGet entry identifiers from the database. More arguments can be given, depending on implementation in specific databases. For mass databases (the ones derived from BiodbBiodbMassdbConn class, the ms.level argument can be set."

	ids = character()

	# Get IDs from volatile cache
	not.null = ! vapply(.self$.entries, is.null, FUN.VALUE = T)
	ids = names(.self$.entries[not.null])

	# Get IDs from database
	if (is.null(max.results) || is.na(max.results) || length(ids) < max.results) {
		db.ids = .self$.doGetEntryIds(if (is.null(max.results)) NA_integer_ else max.results, ...)
		if ( ! is.null(db.ids))
			ids = c(ids, db.ids[ ! db.ids %in% ids])
	}

	# Cut
	if ( ! is.null(max.results) && ! is.na(max.results) && max.results > 0 && length(ids) > max.results)
		ids <- ids[1:max.results]

	return(ids)
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

# Is editable {{{1
################################################################

BiodbConn$methods( isEditable = function() {
	":\n\nReturns TRUE if the database is editable (i.e.: the connector class implements the interface BiodbEditable). If this connector is editable, then you can call allowEditing() to enable editing."

	return(methods::is(.self, 'BiodbEditable'))
})

# Is writable {{{1
################################################################

BiodbConn$methods( isWritable = function() {
	":\n\nReturns TRUE if the database is writable (i.e.: the connector class implements the interface BiodbWritable). If this connector is writable, then you can call allowWriting() to enable writing."

	return(methods::is(.self, 'BiodbWritable'))
})

# Is searchable {{{1
################################################################

BiodbConn$methods( isSearchable = function() {
	":\n\nReturns TRUE if the database is searchable (i.e.: the connector class implements the interface BiodbSearchable)."

	return(methods::is(.self, 'BiodbSearchable'))
})

# Is downloadable {{{1
################################################################

BiodbConn$methods( isDownloadable = function() {
	":\n\nReturns TRUE if the database is downloadable (i.e.: the connector class implements the interface BiodbDownloadable)."

	return(methods::is(.self, 'BiodbDownloadable'))
})

# Is a remote database {{{1
################################################################

BiodbConn$methods( isRemotedb = function() {
	":\n\nReturns TRUE if the database is a remote database (i.e.: the connector class inherits from BiodbRemotedbConn class)."

	return(methods::is(.self, 'BiodbRemotedbConn'))
})

# Is a compound database {{{1
################################################################

BiodbConn$methods( isCompounddb = function() {
	":\n\nReturns TRUE if the database is a compound database (i.e.: the connector class inherits from BiodbCompounddbConn class)."

	return(methods::is(.self, 'BiodbCompounddbConn'))
})

# Is a mass database {{{1
################################################################

BiodbConn$methods( isMassdb = function() {
	":\n\nReturns TRUE if the database is a mass database (i.e.: the connector class inherits from BiodbMassdbConn class)."

	return(methods::is(.self, 'BiodbMassdbConn'))
})

# Show {{{1
################################################################

BiodbConn$methods( show = function() {
	cat("Biodb ", .self$getName(), " connector instance, using URL \"", .self$getUrl('base.url'), "\".\n", sep = '')
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

# Get all cache entries {{{1
################################################################

BiodbConn$methods( getAllCacheEntries = function() {
	":\n\nGet all entries from the memory cache."

	# Remove NULL entries
	entries <- .self$.entries[ ! vapply(.self$.entries, is.null, FUN.VALUE = TRUE)]

	# Remove names
	names(entries) <- NULL

	return(entries)
})

# Delete all cache entries {{{1
################################################################

BiodbConn$methods( deleteAllCacheEntries = function() {
	":\n\nDelete all entries from the memory cache."

	.entries <<- list()
})

# Get cache ID {{{1
################################################################

BiodbConn$methods( getCacheId = function() {
	":\n\nReturns the ID used by this connector in the disk cache."

	id <- NULL

	if ( ! is.null(.self$.cache.id) && ! is.na(.self$.cache.id)) {
		id <- .self$.cache.id

	} else {
		url <- .self$getUrl('base.url')
		if ( ! is.null(url) && ! is.na(url))
			id <- paste(.self$getDbClass(), openssl::md5(url), sep = '-')
	}

	return(id)
})

# Makes reference to entry  {{{1
################################################################

BiodbConn$methods( makesRefToEntry = function(id, db, oid, any = FALSE, recurse = FALSE) {
    'Test for each entry of this database in id parameter if it makes reference to the entry 
    oid from database db. If any is set to TRUE, will return a single logical value: TRUE
    if any entry contains a reference to oid, FALSE otherwise.'

    # Returns TRUE if any entry in id makes reference to oid
    if (any) {
        makes_ref <- FALSE
        for (i in id) {
            e <- .self$getEntry(i)
            if ( ! is.null(e) && e$makesRefToEntry(db=db, oid=oid, recurse=recurse)) {
                makes_ref = TRUE
				break
			}
        }
    }
    
    # Returns a vector, testing each entry in id individually
    else {
        entries <- .self$getEntry(id)
        makes_ref <- vapply(entries,
                           function(e) ! is.null(e) && e$makesRefToEntry(db=db, oid=oid, recurse=recurse),
                           FUN.VALUE=TRUE)
    }
    return(makes_ref)
})

# Private methods {{{1
################################################################

# Do get entry ids {{{2
################################################################

BiodbConn$methods( .doGetEntryIds = function(max.results = NA_integer_) {
	.self$.abstract.method()
})

# Add entries to cache {{{2
################################################################

BiodbConn$methods( .addEntriesToCache = function(ids, entries) {

	ids <- as.character(ids)
	
	names(entries) <- ids

	# Update known entries
	known.ids <- ids[ids %in% names(.self$.entries)] 
	.self$.entries[known.ids] <- entries[ids %in% known.ids]

	# Add new entries
	new.ids <- ids[ ! ids %in% names(.self$.entries)]
	.self$.entries <- c(.self$.entries, entries[ids %in% new.ids])
})

# Get entries from cache {{{2
################################################################

BiodbConn$methods( .getEntriesFromCache = function(ids) {

	ids <- as.character(ids)

	return(.self$.entries[ids])
})

# Get entries missing from cache {{{2
################################################################

BiodbConn$methods( .getEntryMissingFromCache = function(ids) {

	ids <- as.character(ids)

	missing.ids <- ids[ ! ids %in% names(.self$.entries)]

	return(missing.ids)
})

# Get parsing expressions {{{2
################################################################

BiodbConn$methods( .getParsingExpressions = function() {
})
