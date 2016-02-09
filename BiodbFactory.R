if ( ! exists('BiodbFactory')) { # Do not load again if already loaded
	
#	source('ChebiConn.R')
#	source('KeggConn.R')
#	source('PubchemConn.R')
	source('MassbankConn.R')

	#############
	# CONSTANTS #
	#############

	RBIODB.CHEBI <- 'chebi'
	RBIODB.KEGG  <- 'kegg'
	RBIODB.PUBCHEM  <- 'pubchem'
	RBIODB.MASSBANK  <- 'massbank'

	#####################
	# CLASS DECLARATION #
	#####################
	
	BiodbFactory <- setRefClass("BiodbFactory", fields = list(.useragent = "character", .conn = "list"))
	
	###############
	# CONSTRUCTOR #
	###############
	
	BiodbFactory$methods( initialize = function(useragent = NA_character_, ...) {
	
		( ! is.null(useragent) && ! is.na(useragent)) || stop("You must provide a user agent string (e.g.: \"myapp ; my.email@address\").")
		.useragent <<- useragent
		.conn <<- list()

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
		                	chebi = ChebiConn$new(useragent = .self$.useragent),
		                	kegg  = KeggConn$new(useragent = .self$.useragent),
		                	pubchem  = PubchemConn$new(useragent = .self$.useragent),
		                	massbank  = MassbankConn$new(useragent = .self$.useragent),
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

	BiodbFactory$methods( createEntry = function(class, type, id = NULL, content = NULL) {

		is.null(id) && is.null(content) && stop("One of id or content must be set.")
		! is.null(id) && ! is.null(content) && stop("id and content cannot be both set.")

		conn <- .self$getConn(class)
		entry <- if (is.null(id)) conn$createEntry(type = type, content = content) else conn$getEntry(type = type, id = id)

		return(entry)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################

	BiodbFactory$methods( getEntryContent = function(class, type, id) {

		conn <- .self$getConn(class)
		content <- conn$getEntryContent(type, id)

		return(content)
	})
}
