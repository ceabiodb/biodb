# vi: fdm=marker

# Constants {{{1
################################################################

BIODB.BASIC.CLASSES <- c('character', 'integer', 'double', 'logical')

# Entry abstract class {{{1
################################################################

BiodbEntry <- methods::setRefClass("BiodbEntry", contains = "BiodbObject", fields = list(.fields ='list', .biodb = "ANY"))

# Constructor {{{1
################################################################

BiodbEntry$methods( initialize = function(biodb = NULL, ...) {

	.fields <<- list()
	.biodb <<- biodb

	callSuper(...)
})

# Get biodb {{{1
################################################################

BiodbEntry$methods( getBiodb = function() {
	return(.self$.biodb)
})

# Set field value {{{1
################################################################

BiodbEntry$methods(	setFieldValue = function(field, value) {

	class = .self$getFieldClass(field)

	# Secific case to handle objects.
	if ( class ==" object" & !(isS4(value) & methods::is(value, "refClass")))
	  .self$message(MSG.ERROR, paste0('Cannot set a non RC instance to field "', field, '" in BiodEntry.'))
	
	# Check cardinality
	if (class != 'data.frame' && .self$getFieldCardinality(field) == BIODB.CARD.ONE) {
		if (length(value) > 1)
			.self$message(MSG.ERROR, paste0('Cannot set more that one value to single value field "', field, '" in BiodEntry.'))
		if (length(value) == 0)
			.self$message(MSG.ERROR, paste0('Cannot set single value field "', field, '" to an empty vector in BiodEntry.'))
	}

	# Check value class
	if (class %in% c('character', 'double', 'integer', 'logical'))
		value <- as.vector(value, mode = class)

	.self$.fields[[field]] <- value
})

# Get field names {{{1
################################################################

BiodbEntry$methods(	getFieldNames = function(field) {
	return(names(.self$.fields))
})

# Has field {{{1
################################################################

BiodbEntry$methods(	hasField = function(field) {
	return(field %in% names(.self$.fields))
})

# Get field class {{{1
################################################################

BiodbEntry$methods(	getFieldClass = function(field) {

	if ( ! field %in% BIODB.FIELDS[['name']])
		.self$message(MSG.ERROR, paste('Unknown field "', field, "\".", sep = ''))

	field.class <- BIODB.FIELDS[which(field == BIODB.FIELDS[['name']]), 'class']

	return(field.class)
})

# Field has basic class {{{1
################################################################

BiodbEntry$methods(	fieldHasBasicClass = function(field) {
	return(.self$getFieldClass(field) %in% BIODB.BASIC.CLASSES)
})

# Get field cardinality {{{1
################################################################

BiodbEntry$methods(	getFieldCardinality = function(field) {

	if ( ! field %in% BIODB.FIELDS[['name']])
		.self$message(MSG.ERROR, paste0('Unknown field "', field, '" in BiodEntry.'))

	field.card <- BIODB.FIELDS[which(field == BIODB.FIELDS[['name']]), 'cardinality']

	return(field.card)
})

# Get field value {{{1
################################################################

BiodbEntry$methods(	getFieldValue = function(field, compute = TRUE, flatten = FALSE) {

	val <- NULL

	# Check field
	if ( ! field %in% BIODB.FIELDS[['name']])
		.self$message(MSG.ERROR, paste0('Unknown field "', field, '" in BiodEntry.'))

	# Compute field value
	if (compute && ! .self$hasField(field))
		.self$.compute.field(field)

	# Get value
	if (.self$hasField(field))
		val <- .self$.fields[[field]]
	else {
		# Return NULL or NA
		class = .self$getFieldClass(field)
		val <- if (class %in% BIODB.BASIC.CLASSES) as.vector(NA, mode = class) else NULL
	}

	# Flatten: convert atomic values with cardinality > 1 into a string
	if (flatten)
		if (.self$.biodb$fieldIsAtomic(field) && .self$getFieldCardinality(field) != BIODB.CARD.ONE)
			val <- paste(val, collapse = MULTIVAL.FIELD.SEP)

	return(val)
})

# Compute field {{{1
################################################################

BiodbEntry$methods(	.compute.field = function(field = NA_character_) {

	success <- FALSE

	if (.self$getBiodb()$getConfig()$isEnabled(CFG.COMPUTE.FIELDS)) {

		# Set of fields to compute
		fields <- names(BIODB.FIELD.COMPUTING)
		if ( ! is.na(field) && field %in% fields)
			fields <- field

		# Loop on all fields to compute
		for(f in fields) {

			# Skip this field if we already have a value for it
			if (.self$hasField(f))
				next

			# Loop on all databases where we can look for a value
			for (db in BIODB.FIELD.COMPUTING[[f]]) {

				# Have we a reference for this database?
				db.id.field <- paste(db, 'id', sep = '.')
				if ( ! .self$hasField(db.id.field))
					next
				db.id <- .self$getFieldValue(db.id.field, compute = FALSE)
				if ( ! is.na(db.id)) {

					# Get value for this field in the database
					.self$message(MSG.DEBUG, paste("Compute value for field \"", f, "\".", sep = '')) 
					db.entry <- .self$getBiodb()$getFactory()$getEntry(db, id = db.id)

					# Set found value
					if ( ! is.null(db.entry)) {
						.self$setFieldValue(f, db.entry$getFieldValue(f))
						success <- TRUE
						break
					}
				}
			}
		}
	}

	return(success)
})

# Get fields as data frame {{{1
################################################################

# TODO add a limiting option to get some of the fields.
BiodbEntry$methods(	getFieldsAsDataFrame = function(only.atomic = TRUE, compute = TRUE) {
	"Convert the entry into a data frame."

	df <- data.frame(stringsAsFactors = FALSE)

	# Compute fields
	if (compute)
		.self$.compute.field()

	# Loop on all fields
	for (f in names(.self$.fields)) {

		# Ignore non atomic values
		if (only.atomic && ! .self$.biodb$fieldIsAtomic(f))
			next

		v <- .self$getFieldValue(f, flatten = TRUE)

		# Transform vector into data frame
		if (is.vector(v)) {
			v <- as.data.frame(v, stringsAsFactors = FALSE)
			colnames(v) <- f
		}

		# Merge value into data frame
		if (is.data.frame(v) && nrow(v) > 0)
			df <- if (nrow(df) == 0) v else merge(df, v)
	}

	return(df)
})

# DEPRECATED METHODS {{{1
################################################################

BiodbEntry$methods(	getField = function(field) {
	.self$.deprecated.method("getFieldValue()")
	return(.self$getFieldValue(field))
})

BiodbEntry$methods(	setField = function(field, value) {
	.self$.deprecated.method("setFieldValue()")
	.self$setFieldValue(field, value)
})
