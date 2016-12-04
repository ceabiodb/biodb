# vi: fdm=marker

##########################
# CLASS DECLARATION {{{1 #
##########################

BiodbFactory <- methods::setRefClass("BiodbFactory", contains = 'BiodbObject', fields = list(.useragent = "character",
														  .conn = "list",
														  .cache.dir = "character",
														  .cache.mode = "character",
														  .debug = "logical",
														  .chunk.size = "integer",
														  .use.env.var = "logical"))

###############
# CONSTRUCTOR #
###############

BiodbFactory$methods( initialize = function(useragent = NA_character_, cache.dir = NA_character_, cache.mode = BIODB.CACHE.READ.WRITE, debug = FALSE, chunk.size = NA_integer_, use.env.var = FALSE, ...) {

	.useragent <<- useragent
	.conn <<- list()
	.cache.dir <<- cache.dir
	.cache.mode <<- cache.mode
	.debug <<- debug
	.chunk.size <<- as.integer(chunk.size)
	.use.env.var <<- use.env.var

	callSuper(...) # calls super-class initializer with remaining parameters
})

#######################
# PRINT DEBUG MESSAGE #
#######################

BiodbFactory$methods( .print.debug.msg = function(msg) {
	if (.self$.debug)
		.print.msg(msg = msg, class = class(.self))
})

##################
# GET USER AGENT #
##################

BiodbFactory$methods( getUserAgent = function() {
	return(.self$.useragent)
})

##################
# SET USER AGENT #
##################

	BiodbFactory$methods( setUserAgent = function(useragent) {
	"Set useragent of BiodbFactory."
	.useragent <<- useragent
})

###############
# CREATE CONN #
###############

BiodbFactory$methods( createConn = function(class, url = NA_character_, token = NA_character_) {
    " Create connection to databases useful for metabolomics."
	if (class %in% names(.self$.conn))
		stop(paste0('A connection of type ', class, ' already exists. Please use method getConn() to access it.'))

	# Use environment variables
	if (.self$.use.env.var) {
		if (is.na(url))
			url <- .biodb.get.env.var(c(class, 'URL'))
		if (is.na(token))
			token <- .biodb.get.env.var(c(class, 'TOKEN'))
	}

	# Create connection instance
	conn <- switch(class,
		            chebi       = ChebiConn$new(useragent = .self$.useragent, debug = .self$.debug),
		            kegg        = KeggConn$new(useragent = .self$.useragent, debug = .self$.debug),
		            pubchemcomp = PubchemConn$new(useragent = .self$.useragent, db = BIODB.PUBCHEMCOMP, debug = .self$.debug),
		            pubchemsub  = PubchemConn$new(useragent = .self$.useragent, db = BIODB.PUBCHEMSUB, debug = .self$.debug),
		            hmdb        = HmdbConn$new(useragent = .self$.useragent, debug = .self$.debug),
		            chemspider  = ChemspiderConn$new(useragent = .self$.useragent, debug = .self$.debug, token = token),
		            enzyme      = EnzymeConn$new(useragent = .self$.useragent, debug = .self$.debug),
		            lipidmaps   = LipidmapsConn$new(useragent = .self$.useragent, debug = .self$.debug),
		            mirbase     = MirbaseConn$new(useragent = .self$.useragent, debug = .self$.debug),
		            ncbigene    = NcbigeneConn$new(useragent = .self$.useragent, debug = .self$.debug),
		            ncbiccds    = NcbiccdsConn$new(useragent = .self$.useragent, debug = .self$.debug),
		            uniprot     = UniprotConn$new(useragent = .self$.useragent, debug = .self$.debug),
		            massbank    = MassbankConn$new(useragent = .self$.useragent, url = url, debug = .self$.debug),
					massfiledb  = MassFiledbConn$new(file = url, debug = .self$.debug),
					peakforest  = PeakforestConn$new(useragent = .self$.useragent, debug = .self$.debug),
		      	    NULL)

	# Unknown class
	if (is.null(conn))
		stop(paste0("Unknown r-biodb class \"", class,"\"."))

	# Register new class
	.self$.conn[[class]] <- conn

	return (.self$.conn[[class]])
})

############
# GET CONN #
############

BiodbFactory$methods( getConn = function(class) {
	"Get connection to a database."

	if ( ! class %in% names(.self$.conn))
		.self$createConn(class)

	return (.self$.conn[[class]])
})

################
# CREATE ENTRY #
################

BiodbFactory$methods( createEntry = function(class, id = NULL, content = NULL, drop = TRUE) {
	"Create Entry from a database by id."

	is.null(id) && is.null(content) && stop("One of id or content must be set.")
	! is.null(id) && ! is.null(content) && stop("id and content cannot be both set.")

	# Debug
	.self$.print.debug.msg(paste0("Creating ", if (is.null(id)) length(content) else length(id), " entries from ", if (is.null(id)) "contents" else paste("ids", paste(if (length(id) > 10) id[1:10] else id, collapse = ", ")), "..."))

	# Get content
	if ( ! is.null(id))
		content <- .self$getEntryContent(class, id)
	conn <- .self$getConn(class)
	entry <- conn$createEntry(content = content, drop = drop)

	# Set factory
	.self$.print.debug.msg(paste0("Setting factory reference into entries..."))
	for (e in c(entry))
		if ( ! is.null(e))
			e$setFactory(.self)

	return(entry)
})

########################
# GET CACHE FILE PATHS #
########################

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

###########################
# LOAD CONTENT FROM CACHE #
###########################

BiodbFactory$methods( .load.content.from.cache = function(class, id) {

	content <- NULL

	# Read contents from files
	file.paths <- .self$.get.cache.file.paths(class, id)
	content <- lapply(file.paths, function(x) { if (is.na(x)) NA_character_ else ( if (file.exists(x)) paste(readLines(x), collapse = "\n") else NULL )} )

	return(content)
})

############################
# IS CACHE READING ENABLED #
############################

BiodbFactory$methods( .is.cache.reading.enabled = function() {
	return( ! is.na(.self$.cache.dir) && .self$.cache.mode %in% c(BIODB.CACHE.READ.ONLY, BIODB.CACHE.READ.WRITE))
})

############################
# IS CACHE WRITING ENABLED #
############################

BiodbFactory$methods( .is.cache.writing.enabled = function() {
	return( ! is.na(.self$.cache.dir) && .self$.cache.mode %in% c(BIODB.CACHE.WRITE.ONLY, BIODB.CACHE.READ.WRITE))
})

#########################
# SAVE CONTENT TO CACHE #
#########################

BiodbFactory$methods( .save.content.to.cache = function(class, id, content) {

	# Write contents into files
	file.paths <- .self$.get.cache.file.paths(class, id)
	mapply(function(c, f) { if ( ! is.null(c)) writeLines(c, f) }, content, file.paths)
})

#####################
# GET ENTRY CONTENT #
#####################

BiodbFactory$methods( getEntryContent = function(class, id) {

	# Debug
	.self$.print.debug.msg(paste0("Get entry content(s) for ", length(id)," id(s)..."))

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
		.self$.print.debug.msg(paste0(sum(is.na(id)), " entry ids are NA."))
	if (.self$.is.cache.reading.enabled()) {
		.self$.print.debug.msg(paste0(sum( ! is.na(id)) - length(missing.ids), " entry content(s) loaded from cache."))
		if (n.duplicates > 0)
			.self$.print.debug.msg(paste0(n.duplicates, " entry ids, whose content needs to be fetched, are duplicates."))
		.self$.print.debug.msg(paste0(length(missing.ids), " entry content(s) need to be fetched."))
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
				.self$.print.debug.msg(paste0("Now ", length(missing.ids) - length(missing.contents)," id(s) left to be retrieved..."))
		}

		# Merge content and missing.contents
		content[id %in% missing.ids] <- vapply(id[id %in% missing.ids], function(x) missing.contents[missing.ids %in% x], FUN.VALUE = '')
	}

	return(content)
})
