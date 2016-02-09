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

	########################
	# Compound ABSTRACT CLASS #
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
		.self$.fields[[field]] <- value
	})
	
	#############
	# GET FIELD #
	#############
	
	BiodbEntry$methods(	getField = function(field) {
		return(if (field %in% names(.self$.fields)) .self$.fields[[field]] else NULL)
	})
	
	###########
	# FACTORY #
	###########
	
	BiodbEntry$methods(	setFactory = function(factory) {
		is.null(factory) || inherits(factory, "BiodbFactory") || stop("The factory instance must inherit from BiodbFactory class.")
		.factory <<- factory
	})
}
