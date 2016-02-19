if ( ! exists('BiodbEntry')) { # Do not load again if already loaded

	#############
	# CONSTANTS #
	#############

	# Fields
	RBIODB.COMPOUND     <- 'compound'
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

	RBIODB.FIELDS <- data.frame(matrix(c(
		RBIODB.COMPOUND,    'BiodEntry',
		RBIODB.ACCESSION,   'character',
		RBIODB.NAME,        'character',
		RBIODB.CHEBI.ID,    'character',
		RBIODB.KEGG.ID,     'character',
		RBIODB.PUBCHEM.ID,  'character',
		RBIODB.INCHI,       'character',
		RBIODB.MSDEV,       'character',
		RBIODB.MSDEVTYPE,   'character',
		RBIODB.MSTYPE,      'character',
		RBIODB.MSMODE,      'character',
		RBIODB.MSPRECMZ,    'double',
		RBIODB.MSPRECANNOT, 'character'
		), byrow = TRUE, ncol = 2), stringsAsFactors = FALSE)
	colnames(RBIODB.FIELDS) <- c('name', 'class')

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

		if ( ! field %in% RBIODB.FIELDS[['name']])
			stop(paste0('Unknown field "', field, '" in BiodEntry.'))

		field.class <- RBIODB.FIELDS[which(field == RBIODB.FIELDS[['name']]), 'class']
		value <- switch(field.class,
		       'double' = as.double(value),
		       'character' = as.character(value),
		       value)

		.self$.fields[[field]] <- value
	})
	
	#############
	# GET FIELD #
	#############
	
	BiodbEntry$methods(	getField = function(field) {

		if ( ! field %in% RBIODB.FIELDS[['name']])
			stop(paste0('Unknown field "', field, '" in BiodEntry.'))

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
