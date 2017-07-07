# vi: fdm=marker

# Class declaration {{{1
################################################################

#' A class for constructing biodb objects.
#'
#' This class is responsible for the creation of database connectors and database entries. You must go through the single instance of this class to create and get connectors, as well as instantiate entries. To get the single instance of this class, call the \code{getFactory()} method of class \code{Biodb}.
#'
#' @seealso \code{\link{Biodb}}, \code{\link{BiodbConn}}, \code{\link{BiodbEntry}}.
#'
#' @param dbid  The ID of a database. The list of IDs can be obtain from the class \code{\link{BiodbDbsInfo}}.
#' @param url   An URL to the database for which to create a connection. Each database connector is configured with a default URL, but some allow you to change it.
#' @param token A security access token for the database. Some database require such a token for all or some of their webservices. Usually you obtain the token through your account on the database website.
#'
#' @import methods
#' @include ChildObject.R
#' @export BiodbFactory
#' @exportClass BiodbFactory
BiodbFactory <- methods::setRefClass("BiodbFactory", contains = 'ChildObject', fields = list( .conn = "list", .chunk.size = "integer"))

# Constructor {{{1
################################################################

BiodbFactory$methods( initialize = function(...) {

	callSuper(...)

	.conn <<- list()
	.chunk.size <<- NA_integer_
})

# Create conn {{{2
################################################################

BiodbFactory$methods( createConn = function(dbid, url = NA_character_, token = NA_character_) {
    ":\n\nCreate a connection to a database."

    # Has a connection been already created for this database?
	if (dbid %in% names(.self$.conn))
		.self$message(MSG.ERROR, paste0('A connection of type ', dbid, ' already exists. Please use method getConn() to access it.'))

    # Get connection class
    conn.class <- .self$getBiodb()$getDbsInfo()$get(dbid)$getClass()

	# Create connection instance
    if (is.na(url))
    	conn <- conn.class$new(id = dbid, parent = .self)
    else
    	conn <- conn.class$new(id = dbid, parent = .self, base.url = url)

    # Set token
    if ( ! is.na(token))
	    conn$setToken(token)

	# Register new dbid instance
	.self$.conn[[dbid]] <- conn

	return (.self$.conn[[dbid]])
})

# Get conn {{{1
################################################################

BiodbFactory$methods( getConn = function(dbid) {
	":\n\nGet connection to a database."

	if ( ! dbid %in% names(.self$.conn))
		.self$createConn(dbid)

	return (.self$.conn[[dbid]])
})


# Set chunk size {{{1
################################################################

BiodbFactory$methods( setChunkSize = function(size) {
	":\n\n"

	.chunk.size <<- as.integer(size)
})

# Create entry {{{1
################################################################

BiodbFactory$methods( createEntry = function(class, content, drop = TRUE) {

	entries <- list()

	# Check that class is known
	.self$getBiodb()$getDbsInfo()$checkIsDefined(class)

	# Get entry class name
	s <- class
	.self$message(MSG.DEBUG, paste("Create instance for class", class))
	indices <- as.integer(gregexpr('\\.[a-z]', class, perl = TRUE)[[1]])
	indices <- indices + 1  # We are interested in the letter after the dot.
	indices <- c(1, indices) # Add first letter.
	.self$message(MSG.DEBUG, paste("Letters to put in uppercase:", paste(indices, collapse = ", ")))
	for (i in indices)
		s <- paste(substring(s, 1, i - 1), toupper(substring(s, i, i)), substring(s, i + 1), sep = '')
	.self$message(MSG.DEBUG, paste("Create instance for class", s))
	s <- gsub('.', '', s, fixed = TRUE) # Remove dots
	.self$message(MSG.DEBUG, paste("Create instance of class", s))
	entry.class.name <- paste(s, 'Entry', sep = '')

	# Get entry class
	entry.class <- get(entry.class.name)

	# Get connection
	conn <- .self$getConn(class)

    # Loop on all contents
	for (single.content in content) {

		# Create empty entry instance
    	entry <- entry.class$new(parent = conn)

		# Parse content
		if ( ! is.null(single.content) && ! is.na(single.content))
			entry$parseContent(single.content)

		entries <- c(entries, entry)
	}

	# Replace elements with no accession id by NULL
	entries <- lapply(entries, function(x) if (is.na(x$getFieldValue(BIODB.ACCESSION))) NULL else x)

	# If the input was a single element, then output a single object
	if (drop && length(content) == 1)
		entries <- entries[[1]]

	return(entries)
})

# Get entry {{{1
################################################################

BiodbFactory$methods( getEntry = function(class, id, drop = TRUE) {
	"Create Entry from a database by id."

	# Debug
	.self$message(MSG.INFO, paste("Creating", length(id), "entries from ids", paste(if (length(id) > 10) id[1:10] else id, collapse = ", "), "..."))

	# Get contents
	content <- .self$getEntryContent(class, id)

	# Create entries
	entries <- .self$createEntry(class, content = content, drop = drop)

	return(entries)
})

# Get entry content {{{1
################################################################

BiodbFactory$methods( getEntryContent = function(class, id) {

	# Debug
	.self$message(MSG.INFO, paste0("Get ", class, " entry content(s) for ", length(id)," id(s)..."))

	# Download full database if possible
	if (.self$getBiodb()$getCache()$isWritable() && methods::is(.self$getConn(class), 'BiodbDownloadable')) {
		.self$message(MSG.DEBUG, paste('Ask for whole database download of ', class, '.', sep = ''))
		.self$getConn(class)$download()
	}

	# Initialize content
	if (.self$getBiodb()$getCache()$isReadable()) {
		# Load content from cache
		content <- .self$getBiodb()$getCache()$loadFileContent(db = class, folder = CACHE.SHORT.TERM.FOLDER, names = id, ext = .self$getConn(class)$getEntryContentType())
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
		.self$message(MSG.INFO, paste0(sum(is.na(id)), " ", class, " entry ids are NA."))
	if (.self$getBiodb()$getCache()$isReadable()) {
		.self$message(MSG.INFO, paste0(sum( ! is.na(id)) - length(missing.ids), " ", class, " entry content(s) loaded from cache."))
		if (n.duplicates > 0)
			.self$message(MSG.INFO, paste0(n.duplicates, " ", class, " entry ids, whose content needs to be fetched, are duplicates."))
		.self$message(MSG.INFO, paste0(length(missing.ids), " entry content(s) need to be fetched from ", class, " database."))
	}

	# Get contents
	if (length(missing.ids) > 0 && ( ! methods::is(.self$getConn(class), 'BiodbDownloadable') || ! .self$getConn(class)$isDownloaded())) {

		# Use connector to get missing contents
		conn <- .self$getConn(class)

		# Divide list of missing ids in chunks (in order to save in cache regularly)
		chunks.of.missing.ids = if (is.na(.self$.chunk.size)) list(missing.ids) else split(missing.ids, ceiling(seq_along(missing.ids) / .self$.chunk.size))

		# Loop on chunks
		missing.contents <- NULL
		for (ch.missing.ids in chunks.of.missing.ids) {

			ch.missing.contents <- conn$getEntryContent(ch.missing.ids)

			# Save to cache
			if ( ! is.null(ch.missing.contents) && .self$getBiodb()$getCache()$isWritable())
				.self$getBiodb()$getCache()$saveContentToFile(ch.missing.contents, db = class, folder = CACHE.SHORT.TERM.FOLDER, names = ch.missing.ids, ext = .self$getConn(class)$getEntryContentType())

			# Append
			missing.contents <- c(missing.contents, ch.missing.contents)

			# Debug
			if (.self$getBiodb()$getCache()$isReadable())
				.self$message(MSG.INFO, paste0("Now ", length(missing.ids) - length(missing.contents)," id(s) left to be retrieved..."))
		}

		# Merge content and missing.contents
		content[id %in% missing.ids] <- vapply(id[id %in% missing.ids], function(x) missing.contents[missing.ids %in% x], FUN.VALUE = '')
	}

	return(content)
})
