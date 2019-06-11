# vi: fdm=marker ts=4 et cc=80

# Class declaration {{{1
################################################################################

#' A class for constructing biodb objects.
#'
#' This class is responsible for the creation of database connectors and database entries. You must go through the single instance of this class to create and get connectors, as well as instantiate entries. To get the single instance of this class, call the \code{getFactory()} method of class \code{Biodb}.
#'
#' @param content   The content (as character vector) of one or more database entries.
#' @param db.class  The type of a database. The list of types can be obtained from the class \code{\link{BiodbDbsInfo}}.
#' @param conn.id   The identifier of a database connector.
#' @param drop      If set to \code{TRUE} and the list of entries contains only one element, then returns this element instead of the list. If set to \code{FALSE}, then returns always a list.
#' @param id        A character vector containing database entry IDs (accession numbers).
#' @param token     A security access token for the database. Some database require such a token for all or some of their webservices. Usually you obtain the token through your account on the database website.
#' @param url       An URL to the database for which to create a connection. Each database connector is configured with a default URL, but some allow you to change it.
#'
#' @seealso \code{\link{Biodb}}, \code{\link{BiodbConn}}, \code{\link{BiodbEntry}}.
#'
#' @examples
#' # Create a Biodb instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Obtain the factory instance:
#' factory <- mybiodb$getFactory()
#'
#' # Create a connection:
#' conn <- factory$createConn('chebi')
#'
#' # Get a database entry:
#' entry <- factory$getEntry('chebi', id='2528')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @include BiodbChildObject.R
#' @export BiodbFactory
#' @exportClass BiodbFactory
BiodbFactory <- methods::setRefClass("BiodbFactory", contains='BiodbChildObject', fields=list( .conn="list"))

# Initialize {{{1
################################################################################

BiodbFactory$methods( initialize=function(...) {

    callSuper(...)

    .self$.conn <- list()
})

# Create connector {{{1
################################################################################

BiodbFactory$methods( createConn=function(db.class, url=NULL, token=NA_character_, fail.if.exists=TRUE, conn.id=NULL, cache.id=NULL) {
    "Create a connection to a database."

    # Get database info
    db.info <- .self$getBiodb()$getDbsInfo()$get(db.class)

    # Get connector class
    conn.class <- db.info$getConnClass()

    # Set connector ID
    if ( ! is.null(conn.id)) {
        if (conn.id %in% names(.self$.conn))
            .self$message('error', paste0('Connector ID "', conn.id, '" is already used.'))
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
    .self$message('debug', paste0('Creating new connector ', conn.id, ' for database class ', db.class, (if (is.null(url) || is.na(url)) '' else paste0(', with base URL "', url, '"')), '.'))
    prop <- if (is.na(token)) list() else list(token=token)
    conn <- conn.class$new(id=conn.id, cache.id=cache.id, other=db.info, properties=prop, parent=.self)
    if ( ! is.null(url) && ! is.na(url))
        conn$setPropValSlot('urls', 'base.url', url)

    # Check if an identical connector already exists
    .self$.checkConnExists(conn, error=fail.if.exists)

    # Register new instance
    .self$.conn[[conn.id]] <- conn

    return (conn)
})

# Connector exists {{{1
################################################################################

BiodbFactory$methods( connExists=function(conn.id) {
    "Returns TRUE if a connector with this ID exists."

    return(conn.id %in% names(.self$.conn))
})

# Delete connector {{{1
################################################################################

BiodbFactory$methods( deleteConn=function(conn.id=NULL, db.class=NULL) {
    "Delete existing connectors."

    # Remove one connector
    if ( ! is.null(conn.id)) {
        .self$.assertIs(conn.id, 'character')

        if ( ! conn.id %in% names(.self$.conn))
            .self$message('error', paste0('Connector "', conn.id, '" is unknown.'))

        .self$deleteAllCacheEntries(conn.id)
        .self$.conn[[conn.id]]$.terminate()
        .self$.conn[[conn.id]] <- NULL
        .self$message('info', paste0('Connector "', conn.id, '" deleted.'))
    }

    # Remove all connectors of a database class
    else if ( ! is.null(db.class)) {
        .self$.assertIs(db.class, 'character')

        n <- 0
        for (c in .self$.conn)
            if (c$getDbClass() == db.class) {
                .self$deleteConn(conn.id=c$getId())
                n <- n + 1
            }
        if (n == 0)
            .self$message('info', paste0('No connectors of type "', db.class, '" to delete.'))
        else
            .self$message('info', paste0(n, ' connector(s) of type "', db.class, '" deleted.'))
    }

    invisible(NULL)
})

# Get all connectors {{{1
################################################################################

BiodbFactory$methods( getAllConnectors=function() {
    "Returns a list of all created connectors."

    return(.self$.conn)
})

# Delete all connectors {{{1
################################################################################

BiodbFactory$methods( deleteAllConnectors=function() {
    "Delete all connectors."

    # Get all connectors
    connectors <- .self$.conn

    # Loop on all connectors
    for (conn in connectors)
        .self$deleteConn(conn.id=conn$getId())
})

# Get connector {{{1
################################################################################

BiodbFactory$methods( getConn=function(conn.id) {
    "Get the instance of connector database corresponding to the submitted ID or database class."

    .self$.assertNotNull(conn.id)
    .self$.assertIs(conn.id, 'character')

    conn <- NULL

    # Get connector from ID
    if (conn.id %in% names(.self$.conn))
        conn <- .self$.conn[[conn.id]]

    # Does conn.id look like a database class?
    if (is.null(conn) && .self$getBiodb()$getDbsInfo()$isDefined(conn.id)) {

        # Try to find connectors that are of this class
        for (c in .self$.conn)
            if (c$getDbClass() == conn.id)
                conn <- c(conn, c)

        # Create connector
        if  (is.null(conn))
            conn <- .self$createConn(db.class=conn.id)
    }

    if (is.null(conn))
        .self$message('error', paste0('Cannot find connector instance "', conn.id, '".'))

    return(conn)
})


# Get entry {{{1
################################################################################

BiodbFactory$methods( getEntry=function(conn.id, id, drop=TRUE) {
    "Create database entry objects from IDs (accession numbers), for the specified connector."

    id <- as.character(id)

    # Get connector
    conn <- .self$getConn(conn.id)

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
})

# Create new entry {{{1
################################################################################

BiodbFactory$methods( createNewEntry=function(db.class) {
    "Create a new entry from scratch. This entry is not stored in cache."

    # Get database info
    db.info <- .self$getBiodb()$getDbsInfo()$get(db.class)

    # Get entry class
    entry.class <- db.info$getEntryClass()

    # Create entry instance
    entry <- entry.class$new(parent=.self)

    return(entry)
})

# Get all cache entries {{{1
################################################################################

BiodbFactory$methods( getAllCacheEntries=function(conn.id) {
    "Get all entries of a connector from the cache."

    .self$.assertNotNull(conn.id)

    if ( ! conn.id %in% names(.self$.conn))
        .self$message('error', paste0('Connector "', conn.id, '" is unknown.'))

    return(.self$.conn[[conn.id]]$getAllCacheEntries())
})

# Delete all cache entries {{{1
################################################################################

BiodbFactory$methods( deleteAllCacheEntries=function(conn.id) {
    "Delete all entries of a connector from the cache."

    .self$.assertNotNull(conn.id)

    if ( ! conn.id %in% names(.self$.conn))
        .self$message('error', paste0('Connector "', conn.id, '" is unknown.'))

    .self$.conn[[conn.id]]$deleteAllCacheEntries()
})

# Show {{{1
################################################################################

BiodbFactory$methods( show=function() {
    cat("Biodb factory instance.\n")
})

# Private methods {{{1
################################################################################

# Create new entries {{{2
################################################################################

BiodbFactory$methods( .loadEntries=function(conn.id, ids, drop) {

    new.entries <- list()

    if (length(ids) > 0) {

        # Get connector
        conn <- .self$getConn(conn.id)

        # Debug
        .self$message('debug', paste("Creating", length(ids), "entries from ids", paste(if (length(ids) > 10) ids[seq_len(10)] else ids, collapse=", "), "..."))

        # Get contents
        content <- conn$getEntryContent(ids)

        # Create entries
        new.entries <- .self$.createEntryFromContent(conn$getId(), content=content, drop=drop)

        # Store new entries in cache
        conn$.addEntriesToCache(ids, new.entries)
    }

    return(new.entries)
})

# Check if a connector already exists {{{2
################################################################################

BiodbFactory$methods( .checkConnExists=function(new.conn, error) {

    # Loop on all connectors
    for (conn in .self$.conn)
        if (conn$getDbClass() == new.conn$getDbClass()) {

            # Compare base URLs
            bu <- conn$getPropValSlot('urls', 'base.url')
            nbu <- new.conn$getPropValSlot('urls', 'base.url')
            same.url <- if (is.na(bu) || is.na(nbu)) FALSE else normalizePath(bu, mustWork=FALSE) == normalizePath(nbu, mustWork=FALSE)

            # Compare tokens
            tk <- conn$getPropertyValue('token')
            ntk <- new.conn$getPropertyValue('token')
            same.token <- if (is.na(tk) || is.na(ntk)) FALSE else tk == ntk

            # Check
            if (same.url && (is.na(new.conn$getPropertyValue('token')) || same.token))
                .self$message(if (error) 'error' else 'caution', paste0('A connector (', conn$getId(), ') already exists for database ', new.conn$getDbClass(), ' with the same URL (', bu, ')', if ( ! is.na(conn$getPropertyValue('token'))) paste0(' and the same token'), '.'))
        }
})

# Terminate {{{2
################################################################################

BiodbFactory$methods( .terminate=function() {
    .self$deleteAllConnectors()
})

# Create entry from content {{{2
################################################################################

BiodbFactory$methods( .createEntryFromContent=function(conn.id, content, drop=TRUE) {

    entries <- list()

    if (length(content) > 0) {

        # Get connector
        conn <- .self$getConn(conn.id)

        .self$message('debug', paste('Creating ', conn$getPropertyValue('name'), ' entries from ', length(content), ' content(s).', sep=''))

        # Get entry class
        entry.class <- conn$getEntryClass()

        # Loop on all contents
        .self$message('debug', paste('Parsing ', length(content), ' ', conn$getPropertyValue('name'), ' entries.', sep=''))
        i <- 0
        for (single.content in content) {

            # Create empty entry instance
            entry <- entry.class$new(parent=conn)

            # Parse content
            entry$parseContent(single.content)

            entries <- c(entries, entry)

            # Send progress message
            i <- i + 1
            .self$progressMsg(msg='Getting entry contents.', index=i,
                              total=length(content), first=(i == 1))
        }

        # Replace elements with no accession id by NULL
        accessions <- vapply(entries, function(x) x$getFieldValue('accession'),  FUN.VALUE='')
        entries.without.accession <- vapply(accessions, function(a) (is.na(a) || length(grep('^\\s*$', a)) > 0), FUN.VALUE=TRUE)
        .self$message('debug', paste0('Accession numbers: ', paste(accessions, collapse=', '), '.', sep=''))
        if (any(entries.without.accession)) {
            n <- sum(entries.without.accession)
            .self$message('debug', paste('Found', n, if (n > 1) 'entries' else 'entry', 'without an accession number. Set', if (n > 1) 'them' else 'it', 'to NULL.'))
            entries[entries.without.accession] <- list(NULL)
        }

        # If the input was a single element, then output a single object
        if (drop && length(content) == 1)
            entries <- entries[[1]]
    }

    return(entries)
})

