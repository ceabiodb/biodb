# vi: fdm=marker

# Class declaration {{{1
################################################################

#'A class for constructing biodb objects.
#'@export
BiodbFactory <- methods::setRefClass("BiodbFactory", contains = 'BiodbObject', fields = list(
														  .conn = "list",
														  .chunk.size = "integer",
														  .biodb = "ANY"))

# Constructor {{{1
################################################################

BiodbFactory$methods( initialize = function(biodb = NULL, ...) {

	.biodb <<- biodb
	.conn <<- list()
	.chunk.size <<- NA_integer_

	callSuper(...)
})

# Get biodb {{{1
################################################################

BiodbFactory$methods( getBiodb = function() {
	return(.self$.biodb)
})

# CONNECTIONS {{{1

# Create conn {{{2
BiodbFactory$methods( createConn = function(class, url = NA_character_, token = NA_character_) {
    " Create connection to databases useful for metabolomics."

    # Has connection been already created?
	if (class %in% names(.self$.conn))
		.self$message(MSG.ERROR, paste0('A connection of type ', class, ' already exists. Please use method getConn() to access it.'))

    # Check that class is known
    if ( ! class %in% BIODB.DATABASES)
		.self$message(MSG.ERROR, paste("Unknown connection class ", class, ".", sep = ''))

    # Get connection class name
    s <- class
	indices <- as.integer(gregexpr('\\.[a-z]', class, perl = TRUE)[[1]])
    indices <- indices + 1  # We are interested in the letter after the dot.
    indices <- c(1, indices) # Add first letter.
	for (i in indices)
		s <- paste(substring(s, 1, i - 1), toupper(substring(s, i, 1)), substring(s, i + 1), sep = '')
    s <- gsub('.', '', s, fixed = TRUE) # Remove dots
	conn.class.name <- s

    # Get connection class
    conn.class <- get(conn.class.name)

	# Create connection instance
    conn <- conn.class$new(biodb = .self$getBiodb())

	# Set URL
    if ( ! is.na(url))
	    conn$setBaseUrl(url)

    # Set token
    if ( ! is.na(token))
	    conn$setToken(token)

	# Register new class instance
	.self$.conn[[class]] <- conn

	return (.self$.conn[[class]])
})

# Get conn {{{2
################################################################

BiodbFactory$methods( getConn = function(class) {
	"Get connection to a database."

	if ( ! class %in% names(.self$.conn))
		.self$createConn(class)

	return (.self$.conn[[class]])
})

# ENTRIES {{{1

# Set chunk size {{{2
BiodbFactory$methods( setChunkSize = function(size) {
	.chunk.size <<- as.integer(size)
})

# Create entry {{{2
BiodbFactory$methods( createEntry = function(class, id = NULL, content = NULL, drop = TRUE) {
	"Create Entry from a database by id."

	is.null(id) && is.null(content) && stop("One of id or content must be set.")
	! is.null(id) && ! is.null(content) && stop("id and content cannot be both set.")

	# Debug
	.self$message(MSG.INFO, paste0("Creating ", if (is.null(id)) length(content) else length(id), " entries from ", if (is.null(id)) "contents" else paste("ids", paste(if (length(id) > 10) id[1:10] else id, collapse = ", ")), "..."))

	# Get content
	if ( ! is.null(id))
		content <- .self$getEntryContent(class, id)
	conn <- .self$getConn(class)
	entry <- conn$createEntry(content = content, drop = drop)

	return(entry)
})

# Get entry content {{{2
BiodbFactory$methods( getEntryContent = function(class, id) {

	# Debug
	.self$message(MSG.INFO, paste0("Get ", class, " entry content(s) for ", length(id)," id(s)..."))

	# Initialize content
	if (.self$getBiodb()$getCache()$isReadable() && ! .self$getBiodb()$getConfig()$isEnabled(CFG.CACHE.FORCE.DOWNLOAD)) {
		# Load content from cache
		content <- .self$getBiodb()$getCache()$loadFileContent(class, id, .self$getConn(class)$getEntryContentType())
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
		.self$message(MSG.INFO, paste0(length(missing.ids), " entry content(s) need to be fetched for ", class, "."))
	}

	# Get contents
	if (length(missing.ids) > 0) {

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
				.self$getBiodb()$getCache()$saveContentToFile(ch.missing.contents, class, ch.missing.ids, .self$getConn(class)$getEntryContentType())

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
