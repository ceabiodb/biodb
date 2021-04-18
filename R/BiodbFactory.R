#' A class for constructing biodb objects.
#'
#' This class is responsible for the creation of database connectors and
#' database entries. You must go through the single instance of this class to
#' create and get connectors, as well as instantiate entries. To get the single
#' instance of this class, call the \code{getFactory()} method of class
#' \code{Biodb}.
#'
#' @seealso \code{\link{Biodb}}, \code{\link{BiodbConn}} and
#' \code{\link{BiodbEntry}}.
#'
#' @examples
#' # Create a Biodb instance with default settings:
#' mybiodb <- biodb::Biodb()
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
#' @import methods
#' @include BiodbChildObject.R
#' @export BiodbFactory
#' @exportClass BiodbFactory
BiodbFactory <- methods::setRefClass("BiodbFactory",
    contains='BiodbChildObject',
    fields=list(
        .conn="list"
    ),

methods=list(

initialize=function(...) {

    callSuper(...)

    .self$.conn <- list()
},

createConn=function(db.class, url=NULL, token=NA_character_,
                    fail.if.exists=TRUE, get.existing.conn=TRUE,
                    conn.id=NULL, cache.id=NULL) {
    ":\n\nCreates a connector to a database.
    \ndb.class: The type of a database. The list of types can be obtained from
    the class BiodbDbsInfo.
    \nurl: An URL to the database for which to create a connection.  Each
    database connector is configured with a default URL, but some allow you to
    change it.
    \ntoken: A security access token for the database. Some database require
    such a token for all or some of their webservices. Usually you obtain the
    token through your account on the database website.
    \nfail.if.exists: If set to TRUE, the method will fail if a connector for
    \nget.existing.conn: This argument will be used only if fail.if.exists is
    set to FALSE and an identical connector already exists. If it set to TRUE,
    the existing connector instance will be returned, otherwise NULL will be
    returned.
    \nconn.id: If set, this identifier will be used for the new connector. An
    error will be raised in case another connector already exists with this
    identifier.
    \ncache.id: If set, this ID will be used as the cache ID for the new
    connector. An error will be raised in case another connector already exists
    with this cache identifier.
    \nReturned value: An instance of the requested connector class.
    "

    # Get database info
    db.info <- .self$getBiodb()$getDbsInfo()$get(db.class)

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
        if (conn.id %in% names(.self$.conn))
            fatal('Connector ID "%s" is already used.', conn.id)
    }
    else {
        # Create a connector ID
        conn.id <- db.class
        i <- 0
        while (conn.id %in% names(.self$.conn)) {
            i <- i + 1
            conn.id <- paste(db.class, i, sep='.')
        }
    }

    # Create connector instance
    prop <- if (is.na(token)) list() else list(token=token)
    conn <- conn.class$new(id=conn.id, cache.id=cache.id, other=db.info,
                           properties=prop, parent=.self)
    if ( ! is.null(url) && ! is.na(url))
        conn$setPropValSlot('urls', 'base.url', url)

    existingConn <- .self$.getExistingConn(conn)
    # Check if an identical connector already exists
    if (is.null(existingConn)) {
        # Debug message
        surl <- if (is.null(url) || is.na(url)) ''
            else paste0(', with base URL "', url, '"')
        getLogger()$debug('Creating new connector %s for database class %s %s.',
            conn.id, db.class, surl)
        
        # Register new instance
        .self$.conn[[conn.id]] <- conn
    } else {
        conn$.terminate()
        msg <- paste0('A connector (', existingConn$getId(),
                      ') already exists for database ', db.class,
                      (if (is.null(url)) '' else
                          paste0(' with the same URL (', url, ')')),
                      (if (is.na(token)) '' else 'and the same token'), '.')
        .self$message(if (fail.if.exists) 'error' else 'warning', msg)
        conn <- if (get.existing.conn) existingConn else NULL
    }

    return (conn)
},

connExists=function(conn.id) {
    ":\n\nTests if a connector exists.
    \nconn.id: A connector ID.
    \nReturned value: TRUE if a connector with this ID exists, FALSE otherwise.
    "

    return(conn.id %in% names(.self$.conn))
},

deleteConn=function(conn) {
    ":\n\nDeletes an existing connector.
    \nconn.id: A connector instance or a connector ID.
    \nReturned value: None.
    "

    if (methods::is(conn, 'BiodbConn')) {
        .self$deleteConn(conn$getId())
    } else {
        chk::chk_character(conn)

        if ( ! conn %in% names(.self$.conn))
            fatal('Connector "%s" is unknown.', conn)

        .self$deleteAllEntriesFromVolatileCache(conn)
        .self$.conn[[conn]]$.terminate()
        .self$.conn[[conn]] <- NULL
        getLogger()$info('Connector "%s" deleted.', conn)
    }

    return(invisible(NULL))
},

deleteConnByClass=function(db.class) {
    ":\n\nDeletes all existing connectors from a same class.
    \ndb.class: The type of a database. All connectors of this database
    type will be deleted.
    \nReturned value: None.
    "

    chk::chk_character(db.class)

    n <- 0
    for (c in .self$.conn)
        if (c$getDbClass() == db.class) {
            .self$deleteConn(c$getId())
            n <- n + 1
        }
    if (n == 0)
        getLogger()$info('No connectors of type "%s" to delete.', db.class)
    else
        getLogger()$info('%d connector(s) of type "%s" deleted.', n, db.class)

    invisible(NULL)
},

getAllConnectors=function() {
    ":\n\nGets all connectors.
    \nReturned value: A list of all created connectors.
    "

    return(.self$.conn)
},

deleteAllConnectors=function() {
    ":\n\nDeletes all connectors.
    \nReturned value: None.
    "

    # Get all connectors
    connectors <- .self$.conn

    # Loop on all connectors
    for (conn in connectors)
        .self$deleteConn(conn$getId())
},

getConn=function(conn.id, class=TRUE, create=TRUE) {
    ":\n\nGets an instantiated connector instance, or create a new one.
    \nconn.id: An existing connector ID.
    \nclass: If set to TRUE, and \"conn.id\" does not correspond to any
    instantiated connector, then interpret \"conn.id\" as a database class and
    looks for the first instantiated connector of that class.
    \ncreate: If set to TRUE, and \"class\" is also set to TRUE, and no suitable
    instantiated connector was found, then creates a new connector instance of
    the class specified by \"conn.id\".
    \nReturned value: The connector instance corresponding to the connector ID
    or to the database ID submitted (if class \"parameter\" is set to TRUE).
    "

    chk::chk_string(conn.id)
    chk::chk_flag(class)
    chk::chk_flag(create)

    conn <- NULL

    # Get connector from ID
    if (conn.id %in% names(.self$.conn))
        conn <- .self$.conn[[conn.id]]

    # Does conn.id look like a database class?
    if (class && is.null(conn) && .self$getBiodb()$getDbsInfo()$isDefined(conn.id)) {

        # Try to find connectors that are of this class
        for (c in .self$.conn)
            if (c$getDbClass() == conn.id)
                conn <- c(conn, c)

        # Create connector
        if  (is.null(conn) && create)
            conn <- .self$createConn(db.class=conn.id)
    }

    if (is.null(conn))
        fatal('Cannot find connector instance "%s".', conn.id)

    return(conn)
},

getEntry=function(conn.id, id, drop=TRUE) {
    ":\n\nRetrieves database entry objects from IDs (accession numbers), for the
    specified connector.
    \nconn.id: An existing connector ID.
    \nid: A character vector containing database entry IDs (accession numbers).
    \ndrop: If set to TRUE and the list of entries contains only one element,
    then returns this element instead of the list. If set to FALSE, then returns
    always a list.
    \nReturned value: A list of BiodbEntry objects, the same length as `id`. A
    NULL value is put into the list for each invalid ID of `id`.
    "

    id <- as.character(id)

    # Get connector
    conn <- .self$getConn(conn.id, class=FALSE, create=FALSE)

    # Correct IDs
    id <- conn$correctIds(id)

    # What entries are missing from cache?
    missing.ids <- conn$.getEntryMissingFromCache(id)

    if (length(missing.ids) > 0)
        new.entries <- .self$.loadEntries(conn$getId(), missing.ids, drop=FALSE)

    # Get entries
    entries <- unname(conn$.getEntriesFromCache(id))

    # If the input was a single element, then output a single object
    if (drop && length(id) == 1)
        entries <- entries[[1]]

    return(entries)
},

createNewEntry=function(db.class) {
    ":\n\nCreates a new entry from scratch. This entry is not stored in cache.
    \ndb.class: A database ID.
    \nReturned value: A new BiodbEntry object.
    "

    # Get database info
    db.info <- .self$getBiodb()$getDbsInfo()$get(db.class)

    # Get entry class
    entry.class <- db.info$getEntryClass()

    # Create entry instance
    entry <- entry.class$new(parent=.self)

    return(entry)
},

getAllCacheEntries=function(conn.id) {
    ":\n\nFor a connector, gets all entries stored in the cache.
    \nconn.id: A connector ID.
    \nReturned values: A list of BiodbEntry objects.
    "

    chk::chk_string(conn.id)

    if ( ! conn.id %in% names(.self$.conn))
        .self$message('error', paste0('Connector "', conn.id, '" is unknown.'))

    return(.self$.conn[[conn.id]]$getAllCacheEntries())
},

deleteAllEntriesFromVolatileCache=function(conn.id) {
    ":\n\nDeletes all entries stored in the cache of the given connector. This
    method is deprecated, please use deleteAllEntriesFromVolatileCache() instead.
    \nconn.id: A connector ID.
    \nReturned values: None.
    "

    chk::chk_string(conn.id)

    if ( ! conn.id %in% names(.self$.conn))
        .self$message('error', paste0('Connector "', conn.id, '" is unknown.'))

    .self$.conn[[conn.id]]$deleteAllEntriesFromVolatileCache()
},

deleteAllCacheEntries=function(conn.id) { # DEPRECATED
    ":\n\nDeletes all entries stored in the cache of the given connector.
    \nconn.id: A connector ID.
    \nReturned values: None.
    "
    lifecycle::deprecate_soft('1.0.0', 'deleteAllCacheEntries()',
                              "deleteAllEntriesFromVolatileCache()")
    .self$deleteAllCacheEntries(conn.id)
},

show=function() {
    ":\n\nPrints information about this instance.
    \nReturned values: None.
    "

    cat("Biodb factory instance.\n")
},

.loadEntries=function(conn.id, ids, drop) {

    new.entries <- list()

    if (length(ids) > 0) {

        # Get connector
        conn <- .self$getConn(conn.id)

        # Debug
        getLogger()$debug("Creating entries from ids %s.", lst2str(ids))

        # Get contents
        content <- conn$getEntryContent(ids)

        # Create entries
        new.entries <- .self$.createEntryFromContent(conn$getId(),
                                                     content=content, drop=drop)

        # Store new entries in cache
        conn$.addEntriesToCache(ids, new.entries)
    }

    return(new.entries)
},

.getExistingConn=function(new.conn) {

    existingConn <- NULL

    # Loop on all connectors
    for (conn in .self$.conn)
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

.terminate=function() {
    .self$deleteAllConnectors()
},

.createEntryFromContent=function(conn.id, content, drop=TRUE) {

    entries <- list()

    if (length(content) > 0) {

        # Get connector
        conn <- .self$getConn(conn.id)

        getLogger()$debug('Creating %s entries from %d content(s).',
                          conn$getPropertyValue('name'), length(content))

        # Get entry class
        entry.class <- conn$getEntryClass()

        # Loop on all contents
        getLogger()$debug('Parsing %d %s entries.', length(content),
                          conn$getPropertyValue('name'))
        prg <- Progress$new(biodb=.self$getBiodb(),
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
        getLogger()$debug('Accession numbers: %s.', strIds)
        if (any(entries.without.accession)) {
            n <- sum(entries.without.accession)
            getLogger()$debug(paste('Found %d entry/ies without an accession',
                ' number. Set it/them to NULL.', n))
            entries[entries.without.accession] <- list(NULL)
        }

        # If the input was a single element, then output a single object
        if (drop && length(content) == 1)
            entries <- entries[[1]]
    }

    return(entries)
}

))

