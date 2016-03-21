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

		class = .self$getFieldClass(field)

		# Check cardinality
		if (class != 'data.frame' && .self$getFieldCardinality(field) == RBIODB.CARD.ONE && length(value) > 1)
			stop(paste0('Cannot set more that one value to single value field "', field, '" in BiodEntry.'))

		# Check value class
		value <- switch(class,
		       'character' = as.character(value),
		       'double' = as.double(value),
		       'integer' = as.integer(value),
		       'logical' = as.logical(value),
		       value)
		# TODO check value class

		.self$.fields[[field]] <- value
	})

	###################
	# GET FIELD CLASS #
	###################
	
	BiodbEntry$methods(	getFieldClass = function(field) {

		if ( ! field %in% RBIODB.FIELDS[['name']])
			stop(paste0('Unknown field "', field, '" in BiodEntry.'))

		field.class <- RBIODB.FIELDS[which(field == RBIODB.FIELDS[['name']]), 'class']

		return(field.class)
	})

	#########################
	# GET FIELD CARDINALITY #
	#########################
	
	BiodbEntry$methods(	getFieldCardinality = function(field) {

		if ( ! field %in% RBIODB.FIELDS[['name']])
			stop(paste0('Unknown field "', field, '" in BiodEntry.'))

		field.card <- RBIODB.FIELDS[which(field == RBIODB.FIELDS[['name']]), 'cardinality']

		return(field.card)
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

		# Return NULL or NA
		class = .self$getFieldClass(field)
		return(if (class %in% c('character', 'integer', 'double', 'logical')) as.vector(NA, mode = class) else NULL)
	})
	
	#################
	# COMPUTE FIELD #
	##################
	
	BiodbEntry$methods(	.compute.field = function(field) {

		if ( ! is.null(.self$.factory) && field %in% names(RBIODB.FIELD.COMPUTING)) {
			for (db in RBIODB.FIELD.COMPUTING[[field]]) {
				db.id <- .self$getField(paste0(db, 'id'))
				if ( ! is.na(db.id)) {
					db.compound <- .self$.factory$createEntry(db, type = RBIODB.COMPOUND, id = db.id)
					if ( ! is.null(db.compound)) {
						.self$setField(field, db.compound$getField(field))
						return(TRUE)
					}
				}
			}
		}

		return(FALSE)
	})

	###########
	# FACTORY #
	###########
	
	BiodbEntry$methods(	setFactory = function(factory) {

		is.null(factory) || inherits(factory, "BiodbFactory") || stop("The factory instance must inherit from BiodbFactory class.")
		.factory <<- factory
	})
}
