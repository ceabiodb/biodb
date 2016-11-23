if ( ! exists('BiodbEntry')) { # Do not load again if already loaded

	source('biodb-common.R')

	#############
	# CONSTANTS #
	#############

	BIODB.BASIC.CLASSES <- c('character', 'integer', 'double', 'logical')

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
	
	###################
	# SET FIELD VALUE #
	###################

	BiodbEntry$methods(	setFieldValue = function(field, value) {

		class = .self$getFieldClass(field)

		# Secific case to handle objects.
		if ( class ==" object" & !(isS4(value) & is(value, "refClass")))
		  stop(paste0('Cannot set a non RC instance to field "', field, '" in BiodEntry.'))
		
		# Check cardinality
		if (class != 'data.frame' && .self$getFieldCardinality(field) == BIODB.CARD.ONE && length(value) > 1)
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
	# GET FIELD NAMES #
	###################

	BiodbEntry$methods(	getFieldNames = function(field) {
		return(names(.self$.fields))
	})

	#############
	# HAS FIELD #
	#############

	BiodbEntry$methods(	hasField = function(field) {
		return(field %in% names(.self$.fields))
	})

	###################
	# GET FIELD CLASS #
	###################

	BiodbEntry$methods(	getFieldClass = function(field) {

		if ( ! field %in% BIODB.FIELDS[['name']])
			stop(paste0('Unknown field "', field, '" in BiodEntry.'))

		field.class <- BIODB.FIELDS[which(field == BIODB.FIELDS[['name']]), 'class']

		return(field.class)
	})

	#########################
	# FIELD HAS BASIC CLASS #
	#########################

	BiodbEntry$methods(	fieldHasBasicClass = function(field) {
		return(.self$getFieldClass(field) %in% BIODB.BASIC.CLASSES)
	})

	#########################
	# GET FIELD CARDINALITY #
	#########################
	
	BiodbEntry$methods(	getFieldCardinality = function(field) {

		if ( ! field %in% BIODB.FIELDS[['name']])
			stop(paste0('Unknown field "', field, '" in BiodEntry.'))

		field.card <- BIODB.FIELDS[which(field == BIODB.FIELDS[['name']]), 'cardinality']

		return(field.card)
	})
	
	###################
	# GET FIELD VALUE #
	###################
	
	BiodbEntry$methods(	getFieldValue = function(field, compute = TRUE) {

		if ( ! field %in% BIODB.FIELDS[['name']])
			stop(paste0('Unknown field "', field, '" in BiodEntry.'))

		if (field %in% names(.self$.fields))
			return(.self$.fields[[field]])
		else if (compute && .self$.compute.field(field))
			return(.self$.fields[[field]])

		# Return NULL or NA
		class = .self$getFieldClass(field)
		return(if (class %in% BIODB.BASIC.CLASSES) as.vector(NA, mode = class) else NULL)
	})
	
	#################
	# COMPUTE FIELD #
	##################
	
	BiodbEntry$methods(	.compute.field = function(field) {

		if ( ! is.null(.self$.factory) && field %in% names(BIODB.FIELD.COMPUTING)) {
			for (db in BIODB.FIELD.COMPUTING[[field]]) {
				db.id <- .self$getField(paste0(db, 'id'))
				if ( ! is.na(db.id)) {
					db.entry <- .self$.factory$createEntry(db, id = db.id)
					if ( ! is.null(db.entry)) {
						.self$setField(field, db.entry$getField(field))
						return(TRUE)
					}
				}
			}
		}

		return(FALSE)
	})
	
	############################
	# GET FIELDS AS DATA FRAME #
	############################
	###TODO add a limiting option to get some fields.
	BiodbEntry$methods(	getFieldsAsDataFrame = function() {
		df <- data.frame()
		# Loop on all fields
		for (f in names(.self$.fields))

			# If field class is a basic type
			if (.self$getFieldClass(f) %in% c('character', 'logical', 'integer', 'double')  &
				length(.self$getFieldValue(f)) == 1)
				df[1, f] <- .self$getFieldValue(f)

		return(df)
	})

	###########
	# FACTORY #
	###########
	
	BiodbEntry$methods(	setFactory = function(factory) {
		is.null(factory) || inherits(factory, "BiodbFactory") || stop("The factory instance must inherit from BiodbFactory class.")
		.factory <<- factory
	})

	##############
	# DEPRECATED #
	##############

	BiodbEntry$methods(	getField = function(field) {
		return(.self$getFieldValue(field))
	})
	
	BiodbEntry$methods(	setField = function(field, value) {
		.self$setFieldValue(field, value)
	})
}
