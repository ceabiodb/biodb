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
		stop(paste0('Unknown field "', field, '" in BiodEntry.'))

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
		stop(paste0('Unknown field "', field, '" in BiodEntry.'))

	field.card <- BIODB.FIELDS[which(field == BIODB.FIELDS[['name']]), 'cardinality']

	return(field.card)
})

# Get field value {{{1
################################################################

BiodbEntry$methods(	getFieldValue = function(field, compute = TRUE, flatten = FALSE) {

	val <- NULL

	if ( ! field %in% BIODB.FIELDS[['name']])
		.self$message(MSG.ERROR, paste0('Unknown field "', field, '" in BiodEntry.'))

	if (field %in% names(.self$.fields)) {
		val <- .self$.fields[[field]]
	}
	else {
		if (compute && .self$.compute.field(field)) {
			val <- .self$.fields[[field]]
		}
		else {
			# Return NULL or NA
			class = .self$getFieldClass(field)
			val <- if (class %in% BIODB.BASIC.CLASSES) as.vector(NA, mode = class) else NULL
		}
	}

	# Flatten: convert atomic values with cardinality > 1 into a string
	if (flatten)
		if (.self$.biodb$fieldIsAtomic(field) && .self$getFieldCardinality(field) != BIODB.CARD.ONE)
			val <- paste(val, collapse = biodb::MULTIVAL.FIELD.SEP)

	return(val)
})

# Compute field {{{1
################################################################

BiodbEntry$methods(	.compute.field = function(field) {

	if ( ! is.null(.self$.biodb) && field %in% names(BIODB.FIELD.COMPUTING)) {
		for (db in BIODB.FIELD.COMPUTING[[field]]) {
			db.id <- .self$getField(paste0(db, 'id'))
			if ( ! is.na(db.id)) {

				.self$message(MSG.DEBUG, paste("Compute value for field \"", field, "\".", sep = '')) 
				db.entry <- .self$.biodb$getFactory$createEntry(db, id = db.id)
				if ( ! is.null(db.entry)) {
					.self$setField(field, db.entry$getField(field))
					return(TRUE)
				}
			}
		}
	}

	return(FALSE)
})

# Get fields as data frame {{{1
################################################################

# TODO add a limiting option to get some of the fields.
BiodbEntry$methods(	getFieldsAsDataFrame = function(only.atomic = TRUE) {
	"Convert the entry into a data frame."

	df <- data.frame(stringsAsFactors = FALSE)

	# Loop on all fields
	for (f in names(.self$.fields)) {

		v <- .self$getFieldValue(f)

		# Ignore non atomic values
		if (only.atomic && ( ! is.vector(v) || length(v) != 1))
			next

		# Transform vector into data frame
		if (is.vector(v)) {
			v <- as.data.frame(v, stringsAsFactors = FALSE)
			colnames(v) <- f
		}

		# Merge value into data frame
		if (is.data.frame(v))
			df <- if (nrow(df) == 0) v else merge(df, v)
	}

	return(df)
})

# DEPRECATED METHODS {{{1
################################################################

BiodbEntry$methods(	getField = function(field) {
	return(.self$getFieldValue(field))
})

BiodbEntry$methods(	setField = function(field, value) {
	.self$setFieldValue(field, value)
})
