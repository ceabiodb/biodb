if ( ! exists('BiodbFactory')) { # Do not load again if already loaded
	
	library(methods)
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

	#####################
	# CLASS DECLARATION #
	#####################
	
	BiodbFactory <- setRefClass("BiodbFactory", fields = list(.useragent = "character", .conn = "list", .cache.dir = "character"))
	
	###############
	# CONSTRUCTOR #
	###############
	
	BiodbFactory$methods( initialize = function(useragent = NA_character_, cache.dir = NA_character_, ...) {
	
		( ! is.null(useragent) && ! is.na(useragent)) || stop("You must provide a user agent string (e.g.: \"myapp ; my.email@address\").")
		.useragent <<- useragent
		.conn <<- list()
		.cache.dir <<- cache.dir

		callSuper(...) # calls super-class initializer with remaining parameters
	})

	##################
	# GET USER AGENT #
	##################

	BiodbFactory$methods( getUserAgent = function() {
		return(.self$.useragent)
	})

	############
	# GET CONN #
	############

	BiodbFactory$methods( getConn = function(class) {

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
		                	massbank    = MassbankConn$new(useragent = .self$.useragent),
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

		# Get content
		if ( ! is.null(id))
			content <- .self$getEntryContent(class, type, id)

		conn <- .self$getConn(class)
		entry <- conn$createEntry(type = type, content = content, drop = drop)

		# Set factory
		for (e in c(entry))
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
		filenames <- vapply(id, function(x) paste0(class, '-', type, '-', x, '.', ext), FUN.VALUE = '')

		# set file paths
		file.paths <- vapply(filenames, function(x) file.path(.self$.cache.dir, x), FUN.VALUE = '')
	
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
		content <- lapply(file.paths, function(x) { if (file.exists(x)) paste(readLines(x), collapse = "\n") else NULL })

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

	BiodbFactory$methods( getEntryContent = function(class, type, id) {

		content <- NULL
		# Load from cache
		if ( ! is.na(.self$.cache.dir))
			content <- .self$.load.content.from.cache(class, type, id)	

		# Get contents
		missing.content.indexes <- vapply(content, is.null, FUN.VALUE = TRUE)
		missing.ids <- if (is.null(content)) id else id[missing.content.indexes]
		if (length(missing.ids) > 0) {

			# Use connector to get missing contents
			conn <- .self$getConn(class)
			missing.contents <- conn$getEntryContent(type, missing.ids)

			# Save to cache
			if ( ! is.null(missing.contents) && ! is.na(.self$.cache.dir))
				.self$.save.content.to.cache(class, type, missing.ids, missing.contents)

			# Merge content and missing.contents
			if (is.null(content))
				content <- missing.contents
			else
				content[missing.content.indexes] <- missing.contents
		}

		return(content)
	})
}
