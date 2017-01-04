# vi: fdm=marker

# CLASS DECLARATION {{{1
################################################################

BiodbFactory <- methods::setRefClass("BiodbFactory", contains = 'BiodbObject', fields = list(
														  .conn = "list",
														  .cache.dir = "character",
														  .cache.mode = "character",
														  .chunk.size = "integer",
														  .biodb = "ANY"))

# CONSTRUCTOR {{{1
################################################################

BiodbFactory$methods( initialize = function(biodb = NULL, ...) {

	.conn <<- list()
	.cache.mode <<- BIODB.CACHE.READ.WRITE
	.cache.dir <<- NA_character_
	.cache.mode <<- NA_character_
	.chunk.size <<- NA_integer_
	.biodb <<- biodb

	callSuper(...)
})

# CACHE {{{1

# Set cache dir {{{2
BiodbFactory$methods( setCacheDir = function(dir) {
	.cache.dir <<- dir
})

# Set cache mode {{{2
BiodbFactory$methods( setCacheMode = function(mode) {
	mode %in% c(BIODB.CACHE.READ.ONLY, BIODB.CACHE.READ.WRITE, BIODB.CACHE.WRITE.ONLY) || .self$message(MSG.ERROR, paste0("Invalid value \"", mode, "\" for cache mode."))
	.cache.mode <<- mode
})

# Get cache file paths {{{2
BiodbFactory$methods( .get.cache.file.paths = function(class, id) {

	# Get extension
	ext <- .self$getConn(class)$getEntryContentType()

	# Set filenames
	filenames <- vapply(id, function(x) { if (is.na(x)) NA_character_ else paste0(class, '-', x, '.', ext) }, FUN.VALUE = '')

	# set file paths
	file.paths <- vapply(filenames, function(x) { if (is.na(x)) NA_character_ else file.path(.self$.cache.dir, x) }, FUN.VALUE = '')

	# Create cache dir if needed
	if ( ! is.na(.self$.cache.dir) && ! file.exists(.self$.cache.dir))
		dir.create(.self$.cache.dir)

	return(file.paths)
})

# Load content from cache {{{2
BiodbFactory$methods( .load.content.from.cache = function(class, id) {

	content <- NULL

	# Read contents from files
	file.paths <- .self$.get.cache.file.paths(class, id)
	content <- lapply(file.paths, function(x) { if (is.na(x)) NA_character_ else ( if (file.exists(x)) paste(readLines(x), collapse = "\n") else NULL )} )

	return(content)
})

# Is cache reading enabled {{{2
BiodbFactory$methods( .is.cache.reading.enabled = function() {
	return( ! is.na(.self$.cache.dir) && .self$.cache.mode %in% c(BIODB.CACHE.READ.ONLY, BIODB.CACHE.READ.WRITE))
})

# Is cache writing enabled {{{2
BiodbFactory$methods( .is.cache.writing.enabled = function() {
	return( ! is.na(.self$.cache.dir) && .self$.cache.mode %in% c(BIODB.CACHE.WRITE.ONLY, BIODB.CACHE.READ.WRITE))
})

# Save content to cache {{{2
BiodbFactory$methods( .save.content.to.cache = function(class, id, content) {

	# Write contents into files
	file.paths <- .self$.get.cache.file.paths(class, id)
	mapply(function(c, f) { if ( ! is.null(c)) writeLines(c, f) }, content, file.paths)
})

# CONNECTIONS {{{1

# Create conn {{{2
BiodbFactory$methods( createConn = function(class, url = NA_character_, token = NA_character_) {
    " Create connection to databases useful for metabolomics."

	if (class %in% names(.self$.conn))
		stop(paste0('A connection of type ', class, ' already exists. Please use method getConn() to access it.'))

	# Use environment variables
	if (is.na(url))
		url <- .self$getEnvVar(c(class, 'URL'))
	if (is.na(token))
		token <- .self$getEnvVar(c(class, 'TOKEN'))

	# Create connection instance
	conn <- switch(class,
		            chebi       = ChebiConn$new(       biodb = .self$.biodb),
		            kegg        = KeggConn$new(        biodb = .self$.biodb),
		            pubchemcomp = PubchemConn$new(     biodb = .self$.biodb, db = BIODB.PUBCHEMCOMP),
		            pubchemsub  = PubchemConn$new(     biodb = .self$.biodb, db = BIODB.PUBCHEMSUB),
		            hmdb        = HmdbConn$new(        biodb = .self$.biodb),
		            chemspider  = ChemspiderConn$new(  biodb = .self$.biodb, token = token),
		            enzyme      = EnzymeConn$new(      biodb = .self$.biodb),
		            lipidmaps   = LipidmapsConn$new(   biodb = .self$.biodb),
		            mirbase     = MirbaseConn$new(     biodb = .self$.biodb),
		            ncbigene    = NcbigeneConn$new(    biodb = .self$.biodb),
		            ncbiccds    = NcbiccdsConn$new(    biodb = .self$.biodb),
		            uniprot     = UniprotConn$new(     biodb = .self$.biodb),
		            massbank    = MassbankConn$new(    biodb = .self$.biodb, url = url),
					massfiledb  = MassFiledbConn$new(  biodb = .self$.biodb, file = url),
					peakforest  = PeakforestConn$new(  biodb = .self$.biodb),
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
	.self$message(paste0("Creating ", if (is.null(id)) length(content) else length(id), " entries from ", if (is.null(id)) "contents" else paste("ids", paste(if (length(id) > 10) id[1:10] else id, collapse = ", ")), "..."))

	# Get content
	if ( ! is.null(id))
		content <- .self$getEntryContent(class, id)
	conn <- .self$getConn(class)
	entry <- conn$createEntry(content = content, drop = drop)

	# Set factory
	.self$message(paste0("Setting factory reference into entries..."))
	for (e in c(entry))
		if ( ! is.null(e))
			e$setFactory(.self)

	return(entry)
})

# Get entry content {{{2
BiodbFactory$methods( getEntryContent = function(class, id) {

	# Debug
	.self$message(paste0("Get entry content(s) for ", length(id)," id(s)..."))

	# Initialize content
	if (.self$.is.cache.reading.enabled()) {
		content <- .self$.load.content.from.cache(class, id)	
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
		.self$message(paste0(sum(is.na(id)), " entry ids are NA."))
	if (.self$.is.cache.reading.enabled()) {
		.self$message(paste0(sum( ! is.na(id)) - length(missing.ids), " entry content(s) loaded from cache."))
		if (n.duplicates > 0)
			.self$message(paste0(n.duplicates, " entry ids, whose content needs to be fetched, are duplicates."))
		.self$message(paste0(length(missing.ids), " entry content(s) need to be fetched."))
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
			if ( ! is.null(ch.missing.contents) && .self$.is.cache.writing.enabled())
				.self$.save.content.to.cache(class, ch.missing.ids, ch.missing.contents)

			# Append
			missing.contents <- c(missing.contents, ch.missing.contents)

			# Debug
			if (.self$.is.cache.reading.enabled())
				.self$message(paste0("Now ", length(missing.ids) - length(missing.contents)," id(s) left to be retrieved..."))
		}

		# Merge content and missing.contents
		content[id %in% missing.ids] <- vapply(id[id %in% missing.ids], function(x) missing.contents[missing.ids %in% x], FUN.VALUE = '')
	}

	return(content)
})
