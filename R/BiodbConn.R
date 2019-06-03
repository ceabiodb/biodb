# vi: fdm=marker ts=4 et cc=80

# BiodbConn {{{1
################################################################################

#' The mother abstract class of all database connectors.
#'
#' This is the super class of all connector classes. All methods defined here
#' are thus common to all connector classes. Some connector classes inherit
#' directly from this abstract class. Some others inherit from intermediate
#' classes \code{\link{BiodbRemotedbConn}} and \code{\link{BiodbMassdbConn}}.
#' As for all connector concrete classes, you won't have to create an instance
#' of this class directly, but you will instead go through the factory class.
#' However, if you plan to develop a new connector, you will have to call the
#' constructor of this class. See section Fields for a list of the constructor's
#' parameters. Concrete classes may have direct web services methods or other
#' specific methods implemented, in which case they will be described inside the
#' documentation of the concrete class. Please refer to the documentation of
#' each concrete class for more information. The database direct web services
#' methods will be named "ws.*".
#'
#' @field id            The identifier of the connector.
#' @field cache.id      The identifier used in the disk cache.
#'
#' @param count         If set to \code{TRUE} and no straightforward way exists
#'                      to get number of entries, count the output of
#'                      \code{getEntryIds()}.
#' @param entry.id      The identifiers (e.g.: accession numbers) as a
#'                      \code{character vector} of the database entries.
#' @param max.results   The maximum of elements to return from the method.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{BiodbRemotedbConn}},
#'          \code{\link{BiodbMassdbConn}}.
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
BiodbConn <- methods::setRefClass("BiodbConn",
    contains = "BiodbConnBase",

# Fields {{{2
################################################################################

fields = list(
    .id = "character",
    .entries = "list",
    .cache.id = 'character'),

# Public methods {{{2
################################################################################

methods = list(

# Initialize {{{3
################################################################################

initialize = function(id = NA_character_, cache.id = NA_character_, ...) {

    callSuper(...)
    .self$.abstract.class('BiodbConn')

    .self$.assert.is(id, "character")
    .self$.id <- id
    .self$.cache.id <- if (is.null(cache.id)) NA_character_ else cache.id
    .self$.entries <- list()
},

# Get id {{{3
################################################################################

getId = function() {
    "Get the identifier of this connector."

    return(.self$.id)
},

# Get entry {{{3
################################################################################

getEntry = function(id, drop = TRUE) {
    "Return the entry corresponding to this ID. You can pass a vector of IDs,
    and you will get a list of entries."

    return(.self$getBiodb()$getFactory()$getEntry(.self$getId(), id=id,
                                                  drop=drop))
},

# Get cache file {{{3
################################################################################

getCacheFile = function(entry.id) {
    "Get the path to the persistent cache file."

    c <- .self$getBiodb()$getCache()
    fp <- c$getFilePath(.self$getCacheId(), 'shortterm', entry.id,
                        .self$getEntryFileExt())
    if ( ! file.exists(fp))
        fp <- NULL

    return(fp)
},

# Get entry content {{{3
################################################################################

getEntryContent = function(id) {
    "Get the contents of database entries from IDs (accession numbers)."

    content <- list()
    cch <- .self$getBiodb()$getCache()
    nm <- .self$getPropertyValue('name')

    if ( ! is.null(id) && length(id) > 0) {

        id <- as.character(id)

        # Debug
        .self$debug("Get ", nm, " entry content(s) for ", length(id),
                    " id(s)...")

        # Download full database
        if (.self$isDownloadable())
            .self$download()

        # Initialize content
        if (cch$isReadable() && ! is.null(.self$getCacheId())) {
            # Load content from cache
            content = cch$loadFileContent(.self$getCacheId(),
                                          subfolder = 'shortterm', name = id,
                                          ext = .self$getEntryFileExt())
            missing.ids = id[vapply(content, is.null, FUN.VALUE = TRUE)]
        }
        else {
            content <- lapply(id, as.null)
            missing.ids <- id
        }

        # Remove duplicates
        n.duplicates <- sum(duplicated(missing.ids))
        missing.ids <- missing.ids[ ! duplicated(missing.ids)]

        # Debug
        if (any(is.na(id)))
            .self$debug(sum(is.na(id)), " ", nm, " entry ids are NA.")
        if (cch$isReadable()) {
            nld <- sum( ! is.na(id)) - length(missing.ids)
            .self$debug(nld, " ", nm, " entry content(s) loaded from cache.")
            if (n.duplicates > 0)
                .self$debug(n.duplicates, " ", nm, " entry ids, whose content",
                           " needs to be fetched, are duplicates.")
        }

        # Get contents
        if (length(missing.ids) > 0
            && ( ! .self$isDownloadable() || ! .self$isDownloaded())) {

            .self$debug(length(missing.ids), " entry content(s) need to be ",
                       "fetched from ", nm, " database \"",
                       .self$getPropValSlot('urls', 'base.url'), "\".")

            # Divide list of missing ids in chunks
            # (in order to save in cache regularly)
            cs <- .self$getBiodb()$getConfig()$get('dwnld.chunk.size')
            .self$debug('dwnld.chunk.size = ', cs)
            chunks.of.missing.ids = if (is.na(cs)) list(missing.ids)
                else split(missing.ids, ceiling(seq_along(missing.ids) / cs))
            .self$debug(length(chunks.of.missing.ids), ' chunk(s) to download.')

            # Loop on chunks
            missing.contents <- NULL
            for (ch.missing.ids in chunks.of.missing.ids) {

                # Get contents of missing entries
                ec <- .self$getEntryContentFromDb(ch.missing.ids)

                # Save to cache
                if ( ! is.null(ec)
                    && ! is.null(.self$getCacheId()) && cch$isWritable())
                    cch$saveContentToFile(ec,
                                          cache.id = .self$getCacheId(),
                                          subfolder = 'shortterm',
                                          name = ch.missing.ids,
                                          ext = .self$getEntryFileExt())

                # Append
                missing.contents <- c(missing.contents, ec)

                # Debug
                if (cch$isReadable()) {
                    n <- length(missing.ids) - length(missing.contents)
                    .self$debug("Now ", n," id(s) left to be retrieved...")
                }
            }

            # Merge content and missing.contents
            missing.contents = as.list(missing.contents)
            ii <- vapply(id[id %in% missing.ids],
                         function(x) which(missing.ids == x), FUN.VALUE = 1L)
            content[id %in% missing.ids] = missing.contents[ii]
        }
    }

    return(content)
},

# Get entry content from database {{{3
################################################################################

getEntryContentFromDb = function(entry.id) {
    "Get the content of an entry from the database."

    .self$.abstract.method()
},

# Get entry ids {{{3
################################################################################

getEntryIds = function(max.results = NA_integer_, ...) {
    "Get entry identifiers from the database. More arguments can be given,
    depending on implementation in specific databases. For mass databases, the
    ones derived from BiodbBiodbMassdbConn class, the ms.level argument can be
    set."

    ids = character()

    # Get IDs from volatile cache
    not.null = ! vapply(.self$.entries, is.null, FUN.VALUE = T)
    ids = names(.self$.entries[not.null])

    # Get IDs from database
    if (is.null(max.results) || is.na(max.results)
        || length(ids) < max.results) {
        mx <- if (is.null(max.results)) NA_integer_ else max.results
        db.ids = .self$.doGetEntryIds(mx, ...)
        if ( ! is.null(db.ids))
            ids = c(ids, db.ids[ ! db.ids %in% ids])
    }

    # Cut
    if ( ! is.null(max.results) && ! is.na(max.results) && max.results > 0
        && length(ids) > max.results)
        ids <- ids[seq_len(max.results)]

    return(ids)
},

# Get nb entries {{{3
################################################################################

getNbEntries = function(count = FALSE) {
    "Get the number of entries contained in this database."

    n <- NA_integer_

    if (count) {
        ids <- .self$getEntryIds()
        if ( ! is.null(ids))
            n <- length(ids)
    }

    return(n)
},

# Is editable {{{3
################################################################################

isEditable = function() {
    "Returns TRUE if the database is editable (i.e.: the connector class
    implements the interface BiodbEditable). If this connector is editable, then
    you can call allowEditing() to enable editing."

    return(methods::is(.self, 'BiodbEditable'))
},

# Is writable {{{3
################################################################################

isWritable = function() {
    "Returns TRUE if the database is writable (i.e.: the connector class
    implements the interface BiodbWritable). If this connector is writable, then
    you can call allowWriting() to enable writing."

    return(methods::is(.self, 'BiodbWritable'))
},

# Is searchable {{{3
################################################################################

isSearchable = function() {
    "Returns TRUE if the database is searchable (i.e.: the connector class
    implements the interface BiodbSearchable)."

    return(methods::is(.self, 'BiodbSearchable'))
},

# Is downloadable {{{3
################################################################################

isDownloadable = function() {
    "Returns TRUE if the database is downloadable (i.e.: the connector class
    implements the interface BiodbDownloadable)."

    return(methods::is(.self, 'BiodbDownloadable'))
},

# Is a remote database {{{3
################################################################################

isRemotedb = function() {
    "Returns TRUE if the database is a remote database (i.e.: the connector
    class inherits from BiodbRemotedbConn class)."

    return(methods::is(.self, 'BiodbRemotedbConn'))
},

# Is a compound database {{{3
################################################################################

isCompounddb = function() {
    "Returns TRUE if the database is a compound database (i.e.: the connector
    class inherits from BiodbCompounddbConn class)."

    return(methods::is(.self, 'BiodbCompounddbConn'))
},

# Is a mass database {{{3
################################################################################

isMassdb = function() {
    "Returns TRUE if the database is a mass database (i.e.: the connector
    class inherits from BiodbMassdbConn class)."

    return(methods::is(.self, 'BiodbMassdbConn'))
},

# Check database {{{3
################################################################################

checkDb = function() {
    "Check that the database is correct by trying to load all entries."

    # Get IDs
    ids <- .self$getEntryIds()

    # Get entries
    entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), ids)
},

# Get all cache entries {{{3
################################################################################

getAllCacheEntries = function() {
    "Get all entries from the memory cache."

    # Remove NULL entries
    entries <- .self$.entries[ ! vapply(.self$.entries, is.null,
                                        FUN.VALUE = TRUE)]

    # Remove names
    names(entries) <- NULL

    return(entries)
},

# Delete all cache entries {{{3
################################################################################

deleteAllCacheEntries = function() {
    "Delete all entries from the memory cache."

    .self$.entries <- list()
},

# Get cache ID {{{3
################################################################################

getCacheId = function() {
    "Returns the ID used by this connector in the disk cache."

    id <- NULL

    if ( ! is.null(.self$.cache.id) && ! is.na(.self$.cache.id)) {
        id <- .self$.cache.id

    } else {
        url <- .self$getPropValSlot('urls', 'base.url')
        if ( ! is.null(url) && ! is.na(url))
            id <- paste(.self$getDbClass(), openssl::md5(url), sep = '-')
    }

    return(id)
},

# Makes reference to entry  {{{3
################################################################################

makesRefToEntry = function(id, db, oid, any = FALSE, recurse = FALSE) {
    'Test for each entry of this database in id parameter if it makes reference
    to the entry oid from database db. If any is set to TRUE, will return a
    single logical value: TRUE if any entry contains a reference to oid, FALSE
    otherwise.'

    # Returns TRUE if any entry in id makes reference to oid
    if (any) {
        makes_ref <- FALSE
        for (i in id) {
            e <- .self$getEntry(i)
            if ( ! is.null(e)
                && e$makesRefToEntry(db=db, oid=oid, recurse=recurse)) {
                makes_ref = TRUE
                break
            }
        }
    }
    
    # Returns a vector, testing each entry in id individually
    else {
        entries <- .self$getEntry(id, drop = FALSE)
        makes_ref <- vapply(entries,
                           function(e) ! is.null(e)
                           && e$makesRefToEntry(db=db, oid=oid,
                                                recurse=recurse),
                           FUN.VALUE=TRUE)
    }
    return(makes_ref)
},

# Private methods {{{2
################################################################################

# Do get entry ids {{{3
################################################################################

.doGetEntryIds = function(max.results = NA_integer_) {
    .self$.abstract.method()
},

# Add entries to cache {{{3
################################################################################

.addEntriesToCache = function(ids, entries) {

    ids <- as.character(ids)
    
    names(entries) <- ids

    # Update known entries
    known.ids <- ids[ids %in% names(.self$.entries)] 
    .self$.entries[known.ids] <- entries[ids %in% known.ids]

    # Add new entries
    new.ids <- ids[ ! ids %in% names(.self$.entries)]
    .self$.entries <- c(.self$.entries, entries[ids %in% new.ids])
},

# Get entries from cache {{{3
################################################################################

.getEntriesFromCache = function(ids) {

    ids <- as.character(ids)

    return(.self$.entries[ids])
},

# Get entries missing from cache {{{3
################################################################################

.getEntryMissingFromCache = function(ids) {

    ids <- as.character(ids)

    missing.ids <- ids[ ! ids %in% names(.self$.entries)]

    return(missing.ids)
}

))
