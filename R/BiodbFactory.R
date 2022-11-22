#' A class for constructing biodb objects.
#'
#' This class is responsible for the creation of database connectors and
#' database entries. You must go through the single instance of this class to
#' create and get connectors, as well as instantiate entries. To get the single
#' instance of this class, call the \code{getFactory()} method of class
#' \code{BiodbMain}.
#'
#' @seealso \code{\link{BiodbMain}}, \code{\link{BiodbConn}} and
#' \code{\link{BiodbEntry}}.
#'
#' @examples
#' # Create a BiodbMain instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Obtain the factory instance:
#' factory <- mybiodb$getFactory()
#'
#' # Get a compound CSV file database
#' chebi.tsv <- system.file("extdata", "chebi_extract.tsv", package='biodb')
#'
#' # Create a connector:
#' conn <- mybiodb$getFactory()$createConn('comp.csv.file', url=chebi.tsv)
#'
#' # Get a database entry:
#' entry <- conn$getEntry(conn$getEntryIds(1))
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import R6
#' @export
BiodbFactory <- R6::R6Class("BiodbFactory",

public=list(

#' @description
#' New instance initializer. The BiodbFactory class must not be instantiated
#' directly.
#' Instead, call the getFactory() method from the BiodbMain instance.
#' @param bdb The BiodbMain instance.
#' @return Nothing.
initialize=function(bdb) {

    chk::chk_is(bdb, 'BiodbMain')

    private$bdb <- bdb
    private$conn <- list()

    return(invisible(NULL))
},

#' @description
#' Returns the biodb main class instance to which this object is
#'     attached.
#' @return The main biodb instance.
getBiodb=function() {

    return(private$bdb)
},

#' @description
#' Creates a connector to a database.
#' @param db.class The type of a database. The list of types can be
#' obtained from the class BiodbDbsInfo.
#' @param url An URL to the database for which to create a connection.
#' Each database connector is configured with a default URL, but some
#' allow you to change it.
#' @param token A security access token for the database. Some database
#' require such a token for all or some of their webservices. Usually you
#' obtain the token through your account on the database website.
#' @param fail.if.exists If set to TRUE, the method will fail if a
#' connector for
#' @param get.existing.conn This argument will be used only if
#' fail.if.exists is set to FALSE and an identical connector already
#' exists. If it set to TRUE, the existing connector instance will be
#' returned, otherwise NULL will be returned.
#' @param conn.id If set, this identifier will be used for the new
#' connector. An error will be raised in case another connector already
#' exists with this identifier.
#' @param cache.id If set, this ID will be used as the cache ID for the
#' new connector. An error will be raised in case another connector
#' already exists with this cache identifier.
#' @return An instance of the requested connector class.
createConn=function(db.class, url=NULL, token=NA_character_,
    fail.if.exists=TRUE, get.existing.conn=TRUE, conn.id=NULL, cache.id=NULL) {

    # Get database info
    db.info <- private$bdb$getDbsInfo()$get(db.class)

    # Disabled?
    if (db.info$getPropertyValue('disabled')) {
        reason <- db.info$getPropertyValue('disabling.reason')
        warn('The "%s" connector is disabled. %s %s', db.class,
            reason, 'You use it at your own risks.')
    }

    # Get connector class
    conn.class <- db.info$getConnClass()

    # Set connector ID
    if ( ! is.null(conn.id)) {
        if (conn.id %in% names(private$conn))
            error('Connector ID "%s" is already used.', conn.id)
    }
    else {
        # Create a connector ID
        conn.id <- db.class
        i <- 0
        while (conn.id %in% names(private$conn)) {
            i <- i + 1
            conn.id <- paste(db.class, i, sep='.')
        }
    }

    # Create connector instance
    prop <- if (is.na(token)) list() else list(token=token)
    if ( ! is.null(url) && ! is.na(url)) {
        prop[['urls']] <- db.info$getPropertyValue('urls')
        prop[['urls']][['base.url']] <- url
    }
    conn <- conn.class$new(id=conn.id, cache.id=cache.id, other=db.info,
        properties=prop, bdb=private$bdb, db.class=db.class)

    existingConn <- private$getExistingConn(conn)
    # Check if an identical connector already exists
    if (is.null(existingConn)) {
        # Debug message
        surl <- if (is.null(url) || is.na(url)) ''
            else paste0(', with base URL "', url, '"')
        logDebug('Creating new connector %s for database class %s %s.',
            conn.id, db.class, surl)
        
        # Register new instance
        private$conn[[conn.id]] <- conn
    } else {
        conn$.__enclos_env__$private$terminate()
        msg <- paste0('A connector (', existingConn$getId(),
            ') already exists for database ', db.class,
            (if (is.null(url)) '' else
            paste0(' with the same URL (', url, ')')),
            (if (is.na(token)) '' else 'and the same token'), '.')
        if (fail.if.exists) error(msg) else warn(msg)
        conn <- if (get.existing.conn) existingConn else NULL
    }

    return (conn)
},

#' @description
#' Tests if a connector exists.
#' @param conn.id A connector ID.
#' @return TRUE if a connector with this ID exists, FALSE otherwise.
connExists=function(conn.id) {

    return(conn.id %in% names(private$conn))
},

#' @description
#' Deletes an existing connector.
#' @param conn A connector instance or a connector ID.
#' @return Nothing.
deleteConn=function(conn) {

    if (methods::is(conn, 'BiodbConn')) {
        self$deleteConn(conn$getId())
    } else {
        chk::chk_character(conn)

        if ( ! conn %in% names(private$conn))
            error('Connector "%s" is unknown.', conn)

        self$deleteAllEntriesFromVolatileCache(conn)
        private$conn[[conn]]$.__enclos_env__$private$terminate()
        private$conn[[conn]] <- NULL
        logInfo('Connector "%s" deleted.', conn)
    }

    return(invisible(NULL))
},

#' @description
#' Deletes all existing connectors from a same class.
#' @param db.class The type of a database. All connectors of this
#' database type will be deleted.
#' @return Nothing.
deleteConnByClass=function(db.class) {

    chk::chk_character(db.class)

    n <- 0
    for (c in private$conn)
        if (c$getDbClass() == db.class) {
            self$deleteConn(c$getId())
            n <- n + 1
        }
    if (n == 0)
        logInfo('No connectors of type "%s" to delete.', db.class)
    else
        logInfo('%d connector(s) of type "%s" deleted.', n, db.class)

    return(invisible(NULL))
},

#' @description
#' Gets all connectors.
#' @return A list of all created connectors.
getAllConnectors=function() {

    return(private$conn)
},

#' @description
#' Deletes all connectors.
#' @return Nothing.
deleteAllConnectors=function() {

    # Get all connectors
    connectors <- private$conn

    # Loop on all connectors
    for (conn in connectors)
        self$deleteConn(conn$getId())

    return(invisible(NULL))
},

#' @description
#' Gets an instantiated connector instance, or create a new one.
#' @param conn.id An existing connector ID.
#' @param class If set to TRUE, and \"conn.id\" does not correspond to
#' any instantiated connector, then interpret \"conn.id\" as a database
#' class and looks for the first instantiated connector of that class.
#' @param create If set to TRUE, and \"class\" is also set to TRUE, and
#' no suitable instantiated connector was found, then creates a new
#' connector instance of the class specified by \"conn.id\".
#' @return The connector instance corresponding to the connector ID or to
#' the database ID submitted (if class \"parameter\" is set to TRUE).
getConn=function(conn.id, class=TRUE, create=TRUE) {

    chk::chk_string(conn.id)
    chk::chk_flag(class)
    chk::chk_flag(create)

    conn <- NULL

    # Get connector from ID
    if (conn.id %in% names(private$conn))
        conn <- private$conn[[conn.id]]

    # Does conn.id look like a database class?
    if (class && is.null(conn) &&
        private$bdb$getDbsInfo()$isDefined(conn.id)) {

        # Try to find connectors that are of this class
        for (c in private$conn)
            if (c$getDbClass() == conn.id)
                conn <- c(conn, c)

        # Create connector
        if  (is.null(conn) && create)
            conn <- self$createConn(db.class=conn.id)
    }

    if (is.null(conn))
        error('Cannot find connector instance "%s".', conn.id)

    return(conn)
},

#' @description
#' Retrieves database entry objects from IDs (accession numbers), for the
#' specified connector.
#' @param conn.id An existing connector ID.
#' @param id A character vector containing database entry IDs (accession
#' numbers).
#' @param drop If set to TRUE and the list of entries contains only one
#' element, then returns this element instead of the list. If set to
#' FALSE, then returns always a list.
#' @param no.null Set to TRUE to remove NULL entries.
#' @param limit Set to a positive value to limit the number of entries
#' returned.
#' @return A list of BiodbEntry objects, the same length as `id`. A
#' NULL value is put into the list for each invalid ID of `id`.
getEntry=function(conn.id, id, drop=TRUE, no.null=FALSE, limit=0) {

    id <- as.character(id)

    # Get connector
    conn <- self$getConn(conn.id)

    # Correct IDs
    id <- conn$correctIds(id)

    # What entries are missing from cache?
    missing.ids <- conn$.__enclos_env__$private$getEntryMissingFromCache(id)

    if (length(missing.ids) > 0) {
        new.entries <- private$loadEntries(conn$getId(), missing.ids,
            drop=FALSE)
}

    # Get entries
    entries <- unname(conn$.__enclos_env__$private$getEntriesFromCache(id))

    # Remove NULL entries
    if (no.null) {
        null.entries <- vapply(entries, is.null, FUN.VALUE=TRUE)
        entries <- entries[ ! null.entries]
    }

    # Cut
    if (limit > 0 && length(entries) > limit)
        entries <- entries[seq_len(limit)]

    # If the input was a single element, then output a single object
    if (drop && length(id) == 1)
        entries <- entries[[1]]

    return(entries)
},

#' @description
#' Creates a new empty entry object from scratch. This entry is not stored in
#' cache, and is directly attached to the factory instance instead of a
#' particular connector.
#' @param db.class A database ID.
#' @return A new BiodbEntry object.
createNewEntry=function(db.class) {

    # Get database info
    db.info <- private$bdb$getDbsInfo()$get(db.class)

    # Get entry class
    entry.class <- db.info$getEntryClass()

    # Create entry instance
    entry <- entry.class$new(parent=self)

    return(entry)
},

#' @description
#' Creates an entry instance from a content.
#' @param conn.id A valid BiodbConn identifier. 
#' @param content A list or character vector of contents to parse to create the
#' entries.
#' @param drop If set to TRUE
#' @return A list of new BiodbEntry objects.
createEntryFromContent=function(conn.id, content, drop=TRUE) {

    chk::chk_string(conn.id)
    chk::chkor_vld(chk::vld_character(content), chk::vld_list(content))
    chk::chk_flag(drop)

    entries <- list()

    if (length(content) > 0) {

        # Get connector
        conn <- self$getConn(conn.id)

        logDebug('Creating %s entries from %d content(s).',
            conn$getPropertyValue('name'), length(content))

        # Get entry class
        entry.class <- conn$getEntryClass()

        # Loop on all contents
        logDebug('Parsing %d %s entries.', length(content),
            conn$getPropertyValue('name'))
        prg <- Progress$new(biodb=private$bdb,
                            msg='Creating entry instances from contents',
                            total=length(content))
        for (single.content in content) {

            # Progress
            prg$increment()

            # Create empty entry instance
            entry <- entry.class$new(parent=conn)

            # Parse content
            entry$parseContent(single.content)

            entries <- c(entries, entry)
        }

        # Replace elements with no accession id by NULL
        accessions <- vapply(entries, function(x) x$getFieldValue('accession'),
            FUN.VALUE='')
        fct <- function(a) (is.na(a) || length(grep('^\\s*$', a)) > 0)
        entries.without.accession <- vapply(accessions, fct, FUN.VALUE=TRUE)
        strIds <- paste(accessions, collapse=', ')
        logDebug('Accession numbers: %s.', strIds)
        if (any(entries.without.accession)) {
            n <- sum(entries.without.accession)
            logDebug0('Found ', n, ' entry/ies without an accession',
                ' number. Set it/them to NULL.')
            entries[entries.without.accession] <- list(NULL)
        }

        # If the input was a single element, then output a single object
        if (drop && length(content) == 1)
            entries <- entries[[1]]
    }

    return(entries)
},

#' @description
#' For a connector, gets all entries stored in the cache.
#' @param conn.id A connector ID.
#' @return A list of BiodbEntry objects.
getAllCacheEntries=function(conn.id) {

    chk::chk_string(conn.id)

    if ( ! conn.id %in% names(private$conn))
        error('Connector "%s" is unknown.', conn.id)

    return(private$conn[[conn.id]]$getAllCacheEntries())
},

#' @description
#' Deletes all entries stored in the cache of the given connector. This
#' method is deprecated, please use deleteAllEntriesFromVolatileCache()
#' instead.
#' @param conn.id A connector ID.
#' @return Nothing.
deleteAllEntriesFromVolatileCache=function(conn.id) {

    chk::chk_string(conn.id)

    if ( ! conn.id %in% names(private$conn))
        error('Connector "%s" is unknown.', conn.id)

    private$conn[[conn.id]]$deleteAllEntriesFromVolatileCache()

    return(invisible(NULL))
},

#' @description
#' Deletes all entries stored in the cache of the given connector.
#' @param conn.id A connector ID.
#' @return Nothing.
deleteAllCacheEntries=function(conn.id) { # DEPRECATED
    lifecycle::deprecate_soft('1.0.0', 'deleteAllCacheEntries()',
        "deleteAllEntriesFromVolatileCache()")
    self$deleteAllCacheEntries(conn.id)

    return(invisible(NULL))
},

#' @description
#' Prints information about this instance.
#' @return Nothing.
print=function() {

    cat("Biodb factory instance.\n")

    return(invisible(NULL))
}
),

private=list(
    conn=NULL,
    bdb=NULL,

loadEntries=function(conn.id, ids, drop) {

    new.entries <- list()

    if (length(ids) > 0) {

        # Get connector
        conn <- self$getConn(conn.id)

        # Debug
        biodb::logDebug("Creating entries from ids %s.", lst2str(ids))

        # Get contents
        content <- conn$getEntryContent(ids)

        # Create entries
        new.entries <- self$createEntryFromContent(conn$getId(),
            content=content, drop=drop)

        # Store new entries in cache
        conn$.__enclos_env__$private$addEntriesToCache(ids, new.entries)
    }

    return(new.entries)
},

getExistingConn=function(new.conn) {

    existingConn <- NULL

    # Loop on all connectors
    for (conn in private$conn)
        if (conn$getDbClass() == new.conn$getDbClass()) {

            # Compare base URLs
            bu <- conn$getPropValSlot('urls', 'base.url')
            nbu <- new.conn$getPropValSlot('urls', 'base.url')
            same.url <- ( ! is.na(bu) && ! is.na(nbu)
                && normalizePath(bu, mustWork=FALSE)
                == normalizePath(nbu, mustWork=FALSE))

            # Compare tokens
            tk <- conn$getPropertyValue('token')
            ntk <- new.conn$getPropertyValue('token')
            same.token <- (is.na(tk) && is.na(ntk)) || tk == ntk

            # Check
            if (same.url && same.token)
                existingConn <- conn
        }
    
    return(existingConn)
},

terminate=function() {
    self$deleteAllConnectors()
}

))
