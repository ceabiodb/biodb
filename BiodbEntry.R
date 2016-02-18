if ( ! exists('BiodbEntry')) { # Do not load again if already loaded

	#############
	# CONSTANTS #
	#############

	# Fields
	RBIODB.ACCESSION    <- 'accession'
	RBIODB.NAME         <- 'name'
	RBIODB.CHEBI.ID     <- 'chebiid'
	RBIODB.KEGG.ID      <- 'keggid'
	RBIODB.PUBCHEM.ID   <- 'pubchemid'
	RBIODB.INCHI        <- 'inchi'
	RBIODB.MSDEV        <- 'msdev'
	RBIODB.MSDEVTYPE    <- 'msdevtype'
	RBIODB.MSTYPE       <- 'mstype'
	RBIODB.MSMODE       <- 'msmode'
	RBIODB.MSPRECMZ     <- 'msprecmz'       # numeric
	RBIODB.MSPRECANNOT  <- 'msprecannot'

	RBIODB.MSMODE.NEG <- 'neg'
	RBIODB.MSMODE.POS <- 'pos'

	RBIODB.NUMERIC.FIELDS <- c(RBIODB.MSPRECMZ)

	########################
	# ENTRY ABSTRACT CLASS #
	########################
	
	BiodbEntry <- setRefClass("BiodbEntry", fields = list(.fields ='list', .factory = "ANY"))
	
	###############
	# CONSTRUCTOR #
	###############
	
	BiodbEntry$methods( initialize = function(...) {
	
		.fields <<- list()
		.factory <<- NULL
	
		callSuper(...) # calls super-class initializer with remaining parameters
	})
	
	#############
	# SET FIELD #
	#############
	
	BiodbEntry$methods(	setField = function(field, value) {

		if (field %in% RBIODB.NUMERIC.FIELDS)
			value <- as.numeric(value)
		else
			value <- as.character(value)

		.self$.fields[[field]] <- value
	})
	
	#############
	# GET FIELD #
	#############
	
	BiodbEntry$methods(	getField = function(field) {
		return(if (field %in% names(.self$.fields)) .self$.fields[[field]] else NA)
	})
	
	###########
	# FACTORY #
	###########
	
	BiodbEntry$methods(	setFactory = function(factory) {
		is.null(factory) || inherits(factory, "BiodbFactory") || stop("The factory instance must inherit from BiodbFactory class.")
		.factory <<- factory
	})
}
