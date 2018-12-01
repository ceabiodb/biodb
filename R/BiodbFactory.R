# vi: fdm=marker

# Class declaration {{{1
################################################################

#' A class for constructing biodb objects.
#'
#' This class is responsible for the creation of database connectors and database entries. You must go through the single instance of this class to create and get connectors, as well as instantiate entries. To get the single instance of this class, call the \code{getFactory()} method of class \code{Biodb}.
#'
#' @param content   The content (as character vector) of one or more database entries.
#' @param db.class  The type of a database. The list of types can be obtained from the class \code{\link{BiodbDbsInfo}}.
#' @param conn.id   The identifier of a database connector.
#' @param drop      If set to \code{TRUE} and the list of entries contains only one element, then returns this element instead of the list. If set to \code{FALSE}, then returns always a list.
#' @param dwnld.chunk.size The number of entries to download before saving to cache. By default, saving to cache is only down once all requested entries have been downloaded.
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
#' entry <- factory$getEntry('chebi', id = '2528')
#'
#' @import methods
#' @include ChildObject.R
#' @export BiodbFactory
#' @exportClass BiodbFactory
BiodbFactory <- methods::setRefClass("BiodbFactory", contains = 'ChildObject', fields = list( .conn = "list", .entries = "list", .chunk.size = "integer"))

# Constructor {{{1
################################################################

BiodbFactory$methods( initialize = function(...) {

	callSuper(...)

	.conn <<- list()
	.entries <<- list()
	.chunk.size <<- NA_integer_
})

# Create connector {{{1
################################################################

BiodbFactory$methods( createConn = function(db.class, url = NA_character_, token = NA_character_, fail.if.exists = TRUE) {
    ":\n\nCreate a connection to a database."

	# Get database info
	db.info <- .self$getBiodb()$getDbsInfo()$get(db.class)

    # Get connector class
    conn.class <- db.info$getConnClass()

    # Create a connector ID
    conn.id <- db.class
    i <- 0
    while (conn.id %in% names(.self$.conn)) {
	    i <- i + 1
	    conn.id <- paste(db.class, i, sep = '.')
	}

	# Create connector instance
    .self$message('debug', paste0('Creating new connector for database class ', db.class, (if (is.na(url)) '' else paste0(', with base URL "', url, '"')), '.'))
	conn <- conn.class$new(id = conn.id, other = db.info, base.url = url, token = token, parent = .self)

    # Check if an identical connector already exists
    .self$.checkConnExists(conn, error = fail.if.exists)

	# Register new instance
	.self$.conn[[conn.id]] <- conn

	return (conn)
})

# Delete connector {{{1
################################################################

BiodbFactory$methods( deleteConn = function(conn.id = NULL, db.class = NULL) {
    ":\n\nDelete existing connectors."

	# Remove one connector
    if ( ! is.null(conn.id)) {
		.self$.assert.is(conn.id, 'character')

		if ( ! conn.id %in% names(.self$.conn))
			.self$message('error', paste0('Connector "', conn.id, '" is unknown.'))

		.self$deleteAllCacheEntries(conn.id)
		.self$.conn[[conn.id]]$.terminate()
		.self$.conn[[conn.id]] <- NULL
		.self$message('info', paste0('Connector "', conn.id, '" deleted.'))
	}

    # Remove all connectors of a database class
    else if ( ! is.null(db.class)) {
		.self$.assert.is(db.class, 'character')

		n <- 0
		for (c in .self$.conn)
			if (c$getDbClass() == db.class) {
				.self$deleteConn(conn.id = c$getId())
				n <- n + 1
			}
		if (n == 0)
			.self$message('info', paste0('No connectors of type "', db.class, '" to delete.'))
		else
			.self$message('info', paste0(n, ' connector(s) of type "', db.class, '" deleted.'))
	}
})

# Get all connectors {{{1
################################################################

BiodbFactory$methods( getAllConnectors = function() {
    ":\n\nReturns a list of all created connectors."

	return(.self$.conn)
})

# Delete all connectors {{{1
################################################################

BiodbFactory$methods( deleteAllConnectors = function() {
    ":\n\nDelete all connectors."

	# Get all connectors
	connectors <- .self$.conn

	# Loop on all connectors
	for (conn in connectors)
		.self$deleteConn(conn.id = conn$getId())
})

# Get connector {{{1
################################################################

BiodbFactory$methods( getConn = function(conn.id) {
	":\n\nGet the instance of connector database corresponding to the submitted ID or database class."

	.self$.assert.not.null(conn.id)
	.self$.assert.is(conn.id, 'character')

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
			conn <- .self$createConn(conn.id)
	}

	if (is.null(conn))
		.self$message('error', paste0('Cannot find connector instance "', conn.id, '".'))

	return(conn)
})


# Set chunk size {{{1
################################################################

BiodbFactory$methods( setDownloadChunkSize = function(dwnld.chunk.size) {
	":\n\nSet the download chunk size."

	.chunk.size <<- as.integer(dwnld.chunk.size)
})

# Get entry {{{1
################################################################

BiodbFactory$methods( getEntry = function(conn.id, id, drop = TRUE) {
	":\n\nCreate database entry objects from IDs (accession numbers), for the specified connector."

	id <- as.character(id)

	# Get connector
	conn <- .self$getConn(conn.id)

	# What entries are missing from factory cache?
	missing.ids <- .self$.getMissingEntryIds(conn$getId(), id)

	if (length(missing.ids) > 0)
		new.entries <- .self$.createNewEntries(conn$getId(), missing.ids, drop = FALSE)

	# Get entries
	entries <- unname(.self$.getEntries(conn$getId(), id))

	# If the input was a single element, then output a single object
	if (drop && length(id) == 1)
		entries <- entries[[1]]

	return(entries)
})

# Get all cache entries {{{1
################################################################

BiodbFactory$methods( getAllCacheEntries = function(conn.id) {
	":\n\nGet all entries of a connector from the cache."

	.self$.assert.not.null(conn.id)

	if ( ! conn.id %in% names(.self$.conn))
		.self$message('error', paste0('Connector "', conn.id, '" is unknown.'))

	if (conn.id %in% names(.self$.entries))
		return(.self$.entries[[conn.id]])

	return(NULL)
})

# Delete all cache entries {{{1
################################################################

BiodbFactory$methods( deleteAllCacheEntries = function(conn.id) {
	":\n\nDelete all entries of a connector from the cache."

	.self$.assert.not.null(conn.id)

	if ( ! conn.id %in% names(.self$.conn))
		.self$message('error', paste0('Connector "', conn.id, '" is unknown.'))

	if (conn.id %in% names(.self$.entries))
		.self$.entries[[conn.id]] <- NULL
})

# Get entry content {{{1
################################################################

BiodbFactory$methods( getEntryContent = function(conn.id, id) {
	":\n\nGet the contents of database entries from IDs (accession numbers)."

	content <- character(0)

	if ( ! is.null(id) && length(id) > 0) {

		id <- as.character(id)

		# Get connector instance
		conn <- .self$getConn(conn.id)

		# Debug
		.self$message('info', paste0("Get ", conn$getName(), " entry content(s) for ", length(id)," id(s)..."))

		# Download full database if possible and allowed or if required
		if (.self$getBiodb()$getCache()$isWritable() && methods::is(conn, 'BiodbDownloadable')) {
			.self$message('debug', paste('Ask for whole database download of ', conn$getName(), '.', sep = ''))
			conn$download()
		}

		# Initialize content
		if (.self$getBiodb()$getCache()$isReadable()) {
			# Load content from cache
			content <- .self$getBiodb()$getCache()$loadFileContent(conn.id = conn$getId(), subfolder = 'shortterm', name = id, ext = conn$getEntryContentType())
			missing.ids <- id[vapply(content, is.null, FUN.VALUE = TRUE)]
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
			.self$message('info', paste0(sum(is.na(id)), " ", conn$getName(), " entry ids are NA."))
		if (.self$getBiodb()$getCache()$isReadable()) {
			.self$message('info', paste0(sum( ! is.na(id)) - length(missing.ids), " ", conn$getName(), " entry content(s) loaded from cache."))
			if (n.duplicates > 0)
				.self$message('info', paste0(n.duplicates, " ", conn$getName(), " entry ids, whose content needs to be fetched, are duplicates."))
		}

		# Get contents
		if (length(missing.ids) > 0 && ( ! methods::is(conn, 'BiodbDownloadable') || ! conn$isDownloaded())) {

			.self$message('info', paste0(length(missing.ids), " entry content(s) need to be fetched from ", conn$getName(), " database."))

			# Divide list of missing ids in chunks (in order to save in cache regularly)
			chunks.of.missing.ids = if (is.na(.self$.chunk.size)) list(missing.ids) else split(missing.ids, ceiling(seq_along(missing.ids) / .self$.chunk.size))

			# Loop on chunks
			missing.contents <- NULL
			for (ch.missing.ids in chunks.of.missing.ids) {

				ch.missing.contents <- conn$getEntryContent(ch.missing.ids)

				# Save to cache
				if ( ! is.null(ch.missing.contents) && .self$getBiodb()$getCache()$isWritable())
					.self$getBiodb()$getCache()$saveContentToFile(ch.missing.contents, conn.id = conn$getId(), subfolder = 'shortterm', name = ch.missing.ids, ext = conn$getEntryContentType())

				# Append
				missing.contents <- c(missing.contents, ch.missing.contents)

				# Debug
				if (.self$getBiodb()$getCache()$isReadable())
					.self$message('info', paste0("Now ", length(missing.ids) - length(missing.contents)," id(s) left to be retrieved..."))
			}

			# Merge content and missing.contents
			content[id %in% missing.ids] <- vapply(id[id %in% missing.ids], function(x) missing.contents[missing.ids %in% x], FUN.VALUE = '')
		}
	}

	return(content)
})

# Show {{{1
################################################################

BiodbFactory$methods( show = function() {
	cat("Biodb factory instance.\n")
})

# Private methods {{{1
################################################################

# Create new entries {{{2
################################################################

BiodbFactory$methods( .createNewEntries = function(conn.id, ids, drop) {

	new.entries <- list()

	if (length(ids) > 0) {

		# Get connector
		conn <- .self$getConn(conn.id)

		# Debug
		.self$message('info', paste("Creating", length(ids), "entries from ids", paste(if (length(ids) > 10) ids[1:10] else ids, collapse = ", "), "..."))

		# Get contents
		content <- .self$getEntryContent(conn$getId(), ids)

		# Create entries
		new.entries <- .self$.createEntryFromContent(conn$getId(), content = content, drop = drop)

		# Store new entries in cache
		.self$.storeNewEntries(conn$getId(), ids, new.entries)
	}

	return(new.entries)
})

# Create entries db slot {{{2
################################################################

BiodbFactory$methods( .createEntriesDbSlot = function(conn.id) {

	if ( ! conn.id %in% names(.self$.entries))
		.self$.entries[[conn.id]] <- list()
})

# Get entries {{{2
################################################################

BiodbFactory$methods( .getEntries = function(conn.id, ids) {

	ids <- as.character(ids)

	.self$.createEntriesDbSlot(conn.id)

	return(.self$.entries[[conn.id]][ids])
})

# Store new entries {{{2
################################################################

BiodbFactory$methods( .storeNewEntries = function(conn.id, ids, entries) {

	ids <- as.character(ids)

	.self$.createEntriesDbSlot(conn.id)
	
	names(entries) <- ids

	.self$.entries[[conn.id]] <- c(.self$.entries[[conn.id]], entries)
})

# Get missing entry IDs {{{2
################################################################

BiodbFactory$methods( .getMissingEntryIds = function(conn.id, ids) {

	ids <- as.character(ids)

	.self$.createEntriesDbSlot(conn.id)

	missing.ids <- ids[ ! ids %in% names(.self$.entries[[conn.id]])]

	return(missing.ids)
})

# Check if a connector already exists {{{2
################################################################

BiodbFactory$methods( .checkConnExists = function(new.conn, error) {

	# Loop on all connectors
	for (conn in .self$.conn)
		if (conn$getDbClass() == new.conn$getDbClass()) {
			same.url <- if (is.na(conn$getBaseUrl()) || is.na(new.conn$getBaseUrl())) FALSE else normalizePath(conn$getBaseUrl(), mustWork = FALSE) == normalizePath(new.conn$getBaseUrl(), mustWork = FALSE)
			same.token <- if (is.na(conn$getToken()) || is.na(new.conn$getToken())) FALSE else conn$getToken() == new.conn$getToken()
			if (same.url && (is.na(new.conn$getToken()) || same.token))
				.self$message(if (error) 'error' else 'caution', paste0('A connector (', conn$getId(), ') already exists for database ', new.conn$getDbClass(), ' with the same URL (', conn$getBaseUrl(), ')', if ( ! is.na(conn$getToken())) paste0(' and the same token'), '.'))
		}
})

# Terminate {{{2
################################################################

BiodbFactory$methods( .terminate = function() {
	.self$deleteAllConnectors()
})

# Create entry from content {{{2
################################################################

BiodbFactory$methods( .createEntryFromContent = function(conn.id, content, drop = TRUE) {

	entries <- list()

	if (length(content) > 0) {

		# Get connector
		conn <- .self$getConn(conn.id)

		.self$message('info', paste('Creating ', conn$getName(), ' entries from ', length(content), ' content(s).', sep = ''))

		# Get entry class
    	entry.class <- conn$getEntryClass()

    	# Loop on all contents
    	.self$message('debug', paste('Parsing ', length(content), ' ', conn$getName(), ' entries.', sep = ''))
		for (single.content in content) {

			# Create empty entry instance
    		entry <- entry.class$new(parent = conn)

			# Parse content
			if ( ! is.null(single.content) && ! is.na(single.content))
				entry$parseContent(single.content)

			entries <- c(entries, entry)
		}

		# Replace elements with no accession id by NULL
		accessions <- vapply(entries, function(x) x$getFieldValue('accession'),  FUN.VALUE = '')
    	entries.without.accession <- vapply(accessions, function(a) (is.na(a) || length(grep('^\\s*$', a)) > 0), FUN.VALUE = TRUE)
		.self$message('debug', paste0('Accession numbers: ', paste(accessions, collapse = ', '), '.', sep = ''))
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

