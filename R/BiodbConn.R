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
#' The constructor has the following arguments:
#'
#' id: The identifier of the connector.
#'
#' cache.id: The identifier used in the disk cache.
#'
#' @seealso Super class \code{\link{BiodbConnBase}}, \code{\link{BiodbFactory}},
#' \code{\link{BiodbRemotedbConn}} and \code{\link{BiodbMassdbConn}}.
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
#' @import openssl
#' @include BiodbConnBase.R
#' @export BiodbConn
#' @exportClass BiodbConn
BiodbConn <- methods::setRefClass("BiodbConn",
    contains="BiodbConnBase",
    fields=list(
        .id="character",
        .entries="list",
        .cache.id='character'),

methods=list(

initialize=function(id=NA_character_, cache.id=NA_character_, ...) {

    callSuper(...)
    .self$.abstractClass('BiodbConn')

    .self$.assertIs(id, "character")
    .self$.id <- id
    .self$.cache.id <- if (is.null(cache.id)) NA_character_ else cache.id
    .self$.entries <- list()
},

getId=function() {
    ":\n\nGet the identifier of this connector.
    \nReturned value: The identifier of this connector.
    "

    return(.self$.id)
},

correctIds=function(ids) {
    ":\n\nCorrect a vector of IDs by formatting them to the database official format, if required and possible.
    \nids: A character vector of IDs.
    \nReturned values: The vector of IDs corrected.
    "

    return(ids)
},

getEntry=function(id, drop=TRUE, nulls=TRUE) {
    ":\n\nReturn the entry corresponding to this ID. You can pass a vector of IDs,
    and you will get a list of entries.
    \nid: A character vector containing entry identifiers.
    \ndrop: If set to TRUE and only one entry is requrested, then the returned
    value will be a single BiodbEntry object, otherwise it will be a list of 
    BiodbEntry objects.
    \nnulls: If set to TRUE, NULL entries are preserved. This ensures that the
    output list has the same length than the input vector `id`. Otherwise they
    are removed from the final list.
    \nReturned value: A list of BiodbEntry objects, the same size of the vector
    of IDs. The list will contain NULL values for invalid IDs. If drop is set to
    TRUE and only one etrny was requested then a single BiodbEntry is returned
    instead of a list.
    "

    entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), id=id,
                                                  drop=drop)

    if ( ! nulls && is.list(entries))
        entries <- Filter(function(e) ! is.null(e), entries)

    return(entries)
},

getCacheFile=function(entry.id) {
    ":\n\nGet the path to the persistent cache file.
    \nentry.id: The identifiers (e.g.: accession numbers) as a character vector
    of the database entries.
    \nReturned value: A character vector, the same length as the vector of IDs,
    containing the paths to the cache files corresponding to the requested entry
    IDs.
    "

    c <- .self$getBiodb()$getPersistentCache()
    fp <- c$getFilePath(.self$getCacheId(), entry.id, .self$getEntryFileExt())

    return(fp)
},

getEntryContent=function(id) {
    ":\n\nGet the contents of database entries from IDs (accession numbers).
    \nid: A character vector of entry IDs.
    \nReturned values: A character vector containing the contents of the
    requested IDs. If no content is available for an entry ID, then NA will be
    used.
    "

    content <- list()
    cch <- .self$getBiodb()$getPersistentCache()
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
            content <- cch$loadFileContent(.self$getCacheId(),
                                          name=id,
                                          ext=.self$getEntryFileExt())
            missing.ids <- id[vapply(content, is.null, FUN.VALUE=TRUE)]
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
            .self$debug('dwnld.chunk.size=', cs)
            chunks.of.missing.ids <- if (is.na(cs)) list(missing.ids)
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
                                          cache.id=.self$getCacheId(),
                                          name=ch.missing.ids,
                                          ext=.self$getEntryFileExt())

                # Append
                missing.contents <- c(missing.contents, ec)

                # Debug
                if (cch$isReadable()) {
                    n <- length(missing.ids) - length(missing.contents)
                    .self$debug("Now ", n," id(s) left to be retrieved...")
                }
            }

            # Merge content and missing.contents
            missing.contents <- as.list(missing.contents)
            ii <- vapply(id[id %in% missing.ids],
                         function(x) which(missing.ids == x), FUN.VALUE=1L)
            content[id %in% missing.ids] <- missing.contents[ii]
        }
    }

    return(content)
},

getEntryContentFromDb=function(entry.id) {
    ":\n\nGet the contents of entries directly from the database. A direct request or 
    an access to the database will be made in order to retrieve the contents. No
    access to the biodb cache system will be made.
    \nentry.id: A character vector with the IDs of entries to retrieve.
    \nReturned value: A character vector, the same size of entry.id, with
    contents of the requested entries. An NA value will be set for the content
    of each entry for which the retrieval failed.
    "

    .self$.abstractMethod()
},

getEntryIds=function(max.results=NA_integer_, ...) {
    ":\n\nGet entry identifiers from the database. More arguments can be given,
    depending on implementation in specific databases. For mass databases, the
    ones derived from BiodbBiodbMassdbConn class, the ms.level argument can be
    set.
    \nmax.results: The maximum of elements to return from the method.
    \n...: First arguments to be passed to private .doGetEntryIds() method.
    \nReturned value: A character vector containing entry IDs from the database.
    "

    ids <- character()

    # Get IDs from volatile cache
    not.null <- ! vapply(.self$.entries, is.null, FUN.VALUE=T)
    ids <- names(.self$.entries[not.null])

    # Get IDs from database
    if (is.null(max.results) || is.na(max.results)
        || length(ids) < max.results) {
        mx <- if (is.null(max.results)) NA_integer_ else max.results
        db.ids <- .self$.doGetEntryIds(mx, ...)
        db.ids <- as.character(db.ids)
        if ( ! is.null(db.ids))
            ids <- c(ids, db.ids[ ! db.ids %in% ids])
    }

    # Cut
    if ( ! is.null(max.results) && ! is.na(max.results) && max.results > 0
        && length(ids) > max.results)
        ids <- ids[seq_len(max.results)]

    return(ids)
},

getNbEntries=function(count=FALSE) {
    ":\n\nGet the number of entries contained in this database.
    \ncount: If set to TRUE and no straightforward way exists to get number of
    entries, count the output of getEntryIds().
    \nReturned value: The number of entries in the database, as an integer.
    "

    n <- NA_integer_

    if (count) {
        ids <- .self$getEntryIds()
        if ( ! is.null(ids))
            n <- length(ids)
    }

    return(n)
},

isEditable=function() {
    ":\n\nTests if this connector is able to edit the database (i.e.: the
    connector class implements the interface BiodbEditable). If this connector
    is editable, then you can call allowEditing() to enable editing.
    \nReturned value: Returns TRUE if the database is editable.
    "

    return(methods::is(.self, 'BiodbEditable'))
},

isWritable=function() {
    ":\n\nTests if this connector is able to write into the database (i.e.: the
    connector class implements the interface BiodbWritable). If this connector
    is writable, then you can call allowWriting() to enable writing.
    \nReturned value: Returns TRUE if the database is writable.
    "

    return(methods::is(.self, 'BiodbWritable'))
},

isSearchableByField=function(field) {
    ":\n\nTests if a field can be used to search entries when using methods
    searchByName() and searchCompound().
    \nfield: The name of the field.
    \nReturned value: Returns TRUE if the database is searchable using the specified field, FALSE otherwise.
    "

    v <- FALSE

    ef <- .self$getBiodb()$getEntryFields()
    field <- ef$getRealName(field)
    for (sf in .self$getPropertyValue('searchable.fields'))
        if (ef$getRealName(sf) == field) {
            v <- TRUE
            break;
        }

    return(v)
},

searchByName=function(name, max.results=NA_integer_) {
    ":\n\nSearches the database for entries whose name matches the specified
    name.  Returns a character vector of entry IDs.
    \nname: The name to look for.
    \nmax.results: If set, the number of returned IDs is limited to this
    number.
    \nReturned value: A character vector of entry IDs whose name matches the
    requested name.
    "

    if (.self$isCompounddb())
        return(.self$searchCompound(name=name, max.results=max.results))
    else if (.self$isSearchableByField('name'))
        .self$error('This database is declared to be searchable by name, but',
                    ' no implementation has been defined.')

    return(NULL)
},

isDownloadable=function() {
    ":\n\nTests if the connector can download the database (i.e.: the connector
    class implements the interface BiodbDownloadable).
    \nReturned value: Returns TRUE if the database is downloadable.
    "

    return(methods::is(.self, 'BiodbDownloadable'))
},

isRemotedb=function() {
    ":\n\nTests of the connector is connected to a remote database (i.e.: the
    connector class inherits from BiodbRemotedbConn class).
    \nReturned value: Returns TRUE if the database is a remote database."

    return(methods::is(.self, 'BiodbRemotedbConn'))
},

isCompounddb=function() {
    ":\n\nTests if the connector's database is a compound database (i.e.: the
    connector class inherits from BiodbCompounddbConn class).
    \nReturned value: Returns TRUE if the database is a compound database.
    "

    return(methods::is(.self, 'BiodbCompounddbConn'))
},

isMassdb=function() {
    ":\n\nTests if the connector's database is a mass spectra database (i.e.:
    the connector class inherits from BiodbMassdbConn class).
    \nReturned value: Returns TRUE if the database is a mass database.
    "

    return(methods::is(.self, 'BiodbMassdbConn'))
},

checkDb=function() {
    ":\n\nChecks that the database is correct by trying to retrieve all its
    entries.
    \nReturned values: None."

    # Get IDs
    ids <- .self$getEntryIds()

    # Get entries
    entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), ids)
},

getAllCacheEntries=function() {
    ":\n\nGet all entries stored in the memory cache.
    \nReturned value: A list of BiodbEntry instances.
    "

    # Remove NULL entries
    entries <- .self$.entries[ ! vapply(.self$.entries, is.null,
                                        FUN.VALUE=TRUE)]

    # Remove names
    names(entries) <- NULL

    return(entries)
},

deleteAllCacheEntries=function() {
    ":\n\nDelete all entries from the memory cache.
    \nReturned value: None.
    "

    .self$.entries <- list()
},

getCacheId=function() {
    ":\n\nGets the ID used by this connector in the disk cache.
    \nReturned value: The cache ID of this connector.
    "

    id <- NULL

    if ( ! is.null(.self$.cache.id) && ! is.na(.self$.cache.id)) {
        id <- .self$.cache.id

    } else {
        url <- .self$getPropValSlot('urls', 'base.url')
        if ( ! is.null(url) && ! is.na(url))
            id <- paste(.self$getDbClass(), openssl::md5(url), sep='-')
    }

    return(id)
},

makesRefToEntry=function(id, db, oid, any=FALSE, recurse=FALSE) {
    ":\n\nTests if some entry of this database makes reference to another entry
    of another database.
    \nid: A character vector of entry IDs from the connector's database.
    \ndb: Another database connector.
    \noid: A entry ID from database db.
    \nany: If set to TRUE, returns a single logical value: TRUE if any entry
    contains a reference to oid, FALSE otherwise.
    \nrecurse: If set to TRUE, the algorithm will follow all references to
    entries from other databases, to see if it can establish an indirect link
    to `oid`.
    \nReturned value: A logical vector, the same size as `id`, with TRUE for
    each entry making reference to `oid`, and FALSE otherwise.
    "

    # Returns TRUE if any entry in id makes reference to oid
    if (any) {
        makes_ref <- FALSE
        for (i in id) {
            e <- .self$getEntry(i)
            if ( ! is.null(e)
                && e$makesRefToEntry(db=db, oid=oid, recurse=recurse)) {
                makes_ref <- TRUE
                break
            }
        }
    }

    # Returns a vector, testing each entry in id individually
    else {
        entries <- .self$getEntry(id, drop=FALSE)
        makes_ref <- vapply(entries,
                           function(e) ! is.null(e)
                           && e$makesRefToEntry(db=db, oid=oid,
                                                recurse=recurse),
                           FUN.VALUE=TRUE)
    }
    return(makes_ref)
},

makeRequest=function(...) {
    ":\n\nMakes a BiodbRequest instance using the passed parameters, and set
    ifself as the associated connector.
    \n...: Those parameters are passed to the initializer of BiodbRequest.
    \nReturned value: The BiodbRequest instance.
    "

    req <- BiodbRequest(...)

    req$setConn(.self)

    return(req)
},

.doGetEntryIds=function(max.results=NA_integer_) {
    .self$.abstractMethod()
},

.addEntriesToCache=function(ids, entries) {

    ids <- as.character(ids)

    names(entries) <- ids

    # Update known entries
    known.ids <- ids[ids %in% names(.self$.entries)] 
    .self$.entries[known.ids] <- entries[ids %in% known.ids]

    # Add new entries
    new.ids <- ids[ ! ids %in% names(.self$.entries)]
    .self$.entries <- c(.self$.entries, entries[ids %in% new.ids])
},

.getEntriesFromCache=function(ids) {

    ids <- as.character(ids)

    return(.self$.entries[ids])
},

.getEntryMissingFromCache=function(ids) {

    ids <- as.character(ids)

    missing.ids <- ids[ ! ids %in% names(.self$.entries)]

    return(missing.ids)
}

))
