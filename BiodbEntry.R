if ( ! exists('BiodbEntry')) { # Do not load again if already loaded

	source('common.R')

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
	
	BiodbEntry$methods(	getFieldClass = function(field) {

		if ( ! field %in% RBIODB.FIELDS[['name']])
			stop(paste0('Unknown field "', field, '" in BiodEntry.'))

		field.class <- RBIODB.FIELDS[which(field == RBIODB.FIELDS[['name']]), 'class']

		return(field.class)
	})
	
	#############
	# GET FIELD #
	#############
	
	BiodbEntry$methods(	getField = function(field) {

		if ( ! field %in% RBIODB.FIELDS[['name']])
			stop(paste0('Unknown field "', field, '" in BiodEntry.'))

		if (field %in% names(.self$.fields))
			return(.self$.fields[[field]])
		else if (.self$.compute.field(field))
			return(.self$.fields[[field]])

		return(as.vector(NA, mode = .self$getFieldClass(field)))
	})
	
	#################
	# COMPUTE FIELD #
	##################
	
	BiodbEntry$methods(	.compute.field = function(field) {
		return(FALSE)
	})

#	#################
#	# COMPUTE FIELD #
#	#################
#	
#	KeggCompound$methods( .compute.field = function(field) {
#
#		# TODO can we make this algorithm automatic ==> put it inside BiodbEntry, so that when a field is not found we can look for it inside related compounds obtained through known IDs (CHEBI.ID, LIPIDMAPS.ID, ...). ==> define which ID/FIELD must be used for each field that is suspetible to be found like this. Make the search an option, because it can be time consuming.
#		if (field %in% c(RBIODB.INCHI, RBIODB.INCHIKEY)) {
#			if ( ! is.null(.self$.factory)) {
#				chebiid <- .self$getField(RBIODB.CHEBI.ID)
#				if ( ! is.na(chebiid)) {
#					chebi.compound <- .self$.factory$createCompoundFromDb(RBIODB.CHEBI, chebiid)
#					if ( ! is.null(chebi.compound)) {
#						.self$setField(field, chebi.compound$getField(field))
#						return(TRUE)
#					}
#				}
#			}
#		}
#	
#		return(FALSE)
#	})

	###########
	# FACTORY #
	###########
	
	BiodbEntry$methods(	setFactory = function(factory) {

		is.null(factory) || inherits(factory, "BiodbFactory") || stop("The factory instance must inherit from BiodbFactory class.")
		.factory <<- factory
	})
}
