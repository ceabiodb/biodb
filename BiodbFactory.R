if ( ! exists('BiodbFactory')) { # Do not load again if already loaded
	
	library(methods)
	source('biodb-common.R')
	source('ChebiConn.R')
	source('KeggConn.R')
	source('PubchemConn.R')
	source('HmdbConn.R')
	source('ChemspiderConn.R')
	source('EnzymeConn.R')
	source('LipidmapsConn.R')
	source('MirbaseConn.R')
	source('NcbigeneConn.R')
	source('NcbiccdsConn.R')
	source('UniprotConn.R')
	source('MassbankConn.R')
	source('MassFiledbConn.R')

	#####################
	# CLASS DECLARATION #
	#####################
	
	BiodbFactory <- setRefClass("BiodbFactory", fields = list(.useragent = "character", .conn = "list", .cache.dir = "character", .debug = "logical"))
	
	###############
	# CONSTRUCTOR #
	###############
	
	BiodbFactory$methods( initialize = function(useragent = NA_character_, cache.dir = NA_character_, debug = FALSE, ...) {
	
		.useragent <<- useragent
		.conn <<- list()
		.cache.dir <<- cache.dir
		.debug <<- debug

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
		.useragent <<- useragent
	})

	############
	# GET CONN #
	############

	BiodbFactory$methods( getConn = function(class, url = NA_character_) {

		if ( ! class %in% names(.self$.conn)) {

			# Create connection instance
			conn <- switch(class,
		                	chebi       = ChebiConn$new(useragent = .self$.useragent),
		                	kegg        = KeggConn$new(useragent = .self$.useragent),
		                	pubchem     = PubchemConn$new(useragent = .self$.useragent),
		                	hmdb        = HmdbConn$new(useragent = .self$.useragent),
		                	chemspider  = ChemspiderConn$new(useragent = .self$.useragent),
		                	enzyme      = EnzymeConn$new(useragent = .self$.useragent),
		                	lipidmaps   = LipidmapsConn$new(useragent = .self$.useragent),
		                	mirbase     = MirbaseConn$new(useragent = .self$.useragent),
		                	ncbigene    = NcbigeneConn$new(useragent = .self$.useragent),
		                	ncbiccds    = NcbiccdsConn$new(useragent = .self$.useragent),
		                	uniprot     = UniprotConn$new(useragent = .self$.useragent),
		                	massbank    = MassbankConn$new(useragent = .self$.useragent, debug = .self$.debug),
							massfiledb  = MassFiledbConn$new(file = url),
		      	          	NULL)

			# Unknown class
			if (is.null(conn))
				stop(paste0("Unknown r-biodb class \"", class,"\"."))

			.self$.conn[[class]] <- conn
		}

		return (.self$.conn[[class]])
	})

	################
	# CREATE ENTRY #
	################

	BiodbFactory$methods( createEntry = function(class, type, id = NULL, content = NULL, drop = TRUE) {

		is.null(id) && is.null(content) && stop("One of id or content must be set.")
		! is.null(id) && ! is.null(content) && stop("id and content cannot be both set.")

		# Debug
		.self$.print.debug.msg(paste0("Creating entry from ", if (is.null(id)) "content" else paste("id", id), "..."))

		# Get content
		if ( ! is.null(id))
			content <- .self$getEntryContent(class, type, id)
		conn <- .self$getConn(class)
		entry <- conn$createEntry(type = type, content = content, drop = drop)

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

	BiodbFactory$methods( .get.cache.file.paths = function(class, type, id) {

		# Get extension
		ext <- .self$getConn(class)$getEntryContentType(type)

		# Set filenames
		filenames <- vapply(id, function(x) { if (is.na(x)) NA_character_ else paste0(class, '-', type, '-', x, '.', ext) }, FUN.VALUE = '')

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

	BiodbFactory$methods( .load.content.from.cache = function(class, type, id) {

		content <- NULL

		# Read contents from files
		file.paths <- .self$.get.cache.file.paths(class, type, id)
		content <- lapply(file.paths, function(x) { if (is.na(x)) NA_character_ else ( if (file.exists(x)) paste(readLines(x), collapse = "\n") else NULL )} )

		return(content)
	})

	#########################
	# SAVE CONTENT TO CACHE #
	#########################

	BiodbFactory$methods( .save.content.to.cache = function(class, type, id, content) {

		# Write contents into files
		file.paths <- .self$.get.cache.file.paths(class, type, id)
		mapply(function(c, f) { if ( ! is.null(c)) writeLines(c, f) }, content, file.paths)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################

	BiodbFactory$methods( getEntryContent = function(class, type, id, chunk.size = NA_integer_) {

		# Debug
		.self$.print.debug.msg(paste0("Get entry content(s) for ", length(id)," id(s)..."))

		content <- NULL
		# Load from cache
		if ( ! is.na(.self$.cache.dir))
			content <- .self$.load.content.from.cache(class, type, id)	

		# Get list of missing contents
		missing.content.indexes <- vapply(content, is.null, FUN.VALUE = TRUE)
		missing.ids <- if (is.null(content)) id else id[missing.content.indexes]

		# Debug
		if ( ! is.na(.self$.cache.dir)) {
			.self$.print.debug.msg(paste0(length(id) - length(missing.ids), " entry content(s) loaded from cache."))
			.self$.print.debug.msg(paste0(length(missing.ids), " entry content(s) need to be fetched."))
		}

		# Get contents
		if (length(missing.ids) > 0) {

			# Use connector to get missing contents
			conn <- .self$getConn(class)

			# Divide list of missing ids in chunks (in order to save in cache regularly)
			chunks.of.missing.ids = if (is.na(chunk.size)) list(missing.ids) else split(missing.ids, ceiling(seq_along(missing.ids) / chunk.size))

			# Loop on chunks
			missing.contents <- NULL
			for (ch.missing.ids in chunks.of.missing.ids) {

				ch.missing.contents <- conn$getEntryContent(type, ch.missing.ids)

				# Save to cache
				if ( ! is.null(ch.missing.contents) && ! is.na(.self$.cache.dir))
					.self$.save.content.to.cache(class, type, ch.missing.ids, ch.missing.contents)

				# Append
				missing.contents <- c(missing.contents, ch.missing.contents)

				# Debug
				if ( ! is.na(.self$.cache.dir))
					.self$.print.debug.msg(paste0("Now ", length(missing.ids) - length(missing.contents)," id(s) left to be retrieved..."))
			}

			# Merge content and missing.contents
			if (is.null(content))
				content <- missing.contents
			else
				content[missing.content.indexes] <- missing.contents
		}

		return(content)
	})
}
