if ( ! exists('BiodbFactory')) { # Do not load again if already loaded
	
	source('ChebiConn.R')
	source('KeggConn.R')
	source('PubchemConn.R')

	#############
	# CONSTANTS #
	#############

	RBIODB.CHEBI <- 'chebi'
	RBIODB.KEGG  <- 'kegg'
	RBIODB.PUBCHEM  <- 'pubchem'
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	BiodbFactory <- setRefClass("BiodbFactory", fields = list(.useragent = "character", .conn = "list"))
	
	###############
	# CONSTRUCTOR #
	###############
	
	BiodbFactory$methods( initialize = function(useragent = NA_character_, ...) {
	
		.useragent <<- if ( ! is.null(useragent)) useragent else NA_character_
		.conn <<- list()

		callSuper(...) # calls super-class initializer with remaining parameters
	})

	############
	# GET CONN #
	############

	BiodbFactory$methods( getConn = function(class) {

		if ( ! class %in% names(.self$.conn)) {
			 conn <- switch(class,
		                	chebi = ChebiConn$new(useragent = .self$.useragent),
		                	kegg  = KeggConn$new(useragent = .self$.useragent),
		                	pubchem  = PubchemConn$new(useragent = .self$.useragent),
		      	          	NULL)

			if (is.null(conn))
				stop(paste0("Unknown r-biodb class \"", class,"\"."))

			.self$.conn[[class]] <- conn
		}

		return (.self$.conn[[class]])
	})

	########################
	# CREATE ENTRY FROM DB #
	########################

	BiodbFactory$methods( createEntryFromDb = function(class, id) {

		conn <- .self$getConn(class)
		entry <- conn$getEntry(id = id, factory = .self)

		return(entry)
	})

	#############################
	# CREATE ENTRY FROM CONTENT #
	#############################

	BiodbFactory$methods( createEntryFromContent = function(class, content) {

		conn <- .self$getConn(class)
		entry <- conn$createEntry(content = content, factory = .self)

		return(entry)
	})
}
