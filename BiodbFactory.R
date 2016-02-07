if ( ! exists('BiodbFactory')) { # Do not load again if already loaded
	
	source('ChebiConn.R')
	source('KeggConn.R')
	source('PubchemConn.R')
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
		                	chebi = ChebiConn$new(factory = .self),
		                	kegg  = KeggConn$new(factory = .self),
		                	pubchem  = PubchemConn$new(factory = .self),
		                	massbank  = MassbankConn$new(factory = .self),
		      	          	NULL)

			# Unknown class
			if (is.null(conn))
				stop(paste0("Unknown r-biodb class \"", class,"\"."))

			.self$.conn[[class]] <- conn
		}

		return (.self$.conn[[class]])
	})

	###########################
	# CREATE COMPOUND FROM DB #
	###########################

	BiodbFactory$methods( createCompoundFromDb = function(class, id) {

		conn <- .self$getConn(class)
		compound <- conn$getCompound(id = id)

		return(compound)
	})

	################################
	# CREATE COMPOUND FROM CONTENT #
	################################

	BiodbFactory$methods( createCompoundFromContent = function(class, content) {

		conn <- .self$getConn(class)
		compound <- conn$createCompound(content = content)

		return(compound)
	})
}
