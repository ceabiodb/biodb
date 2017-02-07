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

	if (class %in% names(.self$.conn))
		stop(paste0('A connection of type ', class, ' already exists. Please use method getConn() to access it.'))

	# Create connection instance
	conn <- switch(class,
		            chebi           = ChebiConn$new(            biodb = .self$.biodb),
		            keggcompound    = KeggcompoundConn$new(     biodb = .self$.biodb),
		            pubchemcomp     = PubchemConn$new(          biodb = .self$.biodb, db = BIODB.PUBCHEMCOMP),
		            pubchemsub      = PubchemConn$new(          biodb = .self$.biodb, db = BIODB.PUBCHEMSUB),
		            hmdbmetabolite  = HmdbmetaboliteConn$new(   biodb = .self$.biodb),
		            chemspider      = ChemspiderConn$new(       biodb = .self$.biodb, token = if (is.na(token)) .self$getBiodb()$getConfig()$get(CFG.CHEMSPIDER.TOKEN) else token),
		            enzyme          = EnzymeConn$new(           biodb = .self$.biodb),
		            lipidmaps       = LipidmapsConn$new(        biodb = .self$.biodb),
		            mirbase         = MirbaseConn$new(          biodb = .self$.biodb),
		            ncbigene        = NcbigeneConn$new(         biodb = .self$.biodb),
		            ncbiccds        = NcbiccdsConn$new(         biodb = .self$.biodb),
		            uniprot         = UniprotConn$new(          biodb = .self$.biodb),
		            massbank        = MassbankConn$new(         biodb = .self$.biodb, url = if (is.na(url)) .self$getBiodb()$getConfig()$get(CFG.MASSBANK.URL) else url),
					massfiledb      = MassFiledbConn$new(       biodb = .self$.biodb, file = url),
					peakforest      = PeakforestConn$new(       biodb = .self$.biodb, url = if (is.na(url)) .self$getBiodb()$getConfig()$get(CFG.PEAKFOREST.URL) else url, token = if (is.na(token)) .self$getBiodb()$getConfig()$get(CFG.PEAKFOREST.TOKEN) else token),
		      	    NULL)

	# Unknown class
	if (is.null(conn))
		stop(paste0("Unknown r-biodb class \"", class,"\"."))

	# Register new class
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
	if (.self$getBiodb()$getCache()$isReadable()) {
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
