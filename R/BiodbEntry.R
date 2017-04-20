# vi: fdm=marker

#' @include ChildObject.R

# Constants {{{1
################################################################

BIODB.BASIC.CLASSES <- c('character', 'integer', 'double', 'logical')

# Entry abstract class {{{1
################################################################

BiodbEntry <- methods::setRefClass("BiodbEntry", contains = "ChildObject", fields = list(.fields ='list', .parsing.expr = 'ANY'))

# Constructor {{{1
################################################################

BiodbEntry$methods( initialize = function(...) {

	callSuper(...)

	.fields <<- list()
	.parsing.expr <<- list()
})

# Set field value {{{1
################################################################

BiodbEntry$methods(	setFieldValue = function(field, value) {

	field.def = .self$getBiodb()$getEntryFields()$get(field)

	# Remove duplicates
	if ( ! field.def$allowsDuplicates() && is.vector(value))
		value <- value[ ! duplicated(value)]

	# Secific case to handle objects.
	if (field.def$isObject() && !(isS4(value) & methods::is(value, "refClass")))
	  .self$message(MSG.ERROR, paste0('Cannot set a non RC instance to field "', field, '" in BiodEntry.'))
	
	# Check cardinality
	if (field.def$isDataFrame() && field.def$hasCardOne()) {
		if (length(value) > 1)
			.self$message(MSG.ERROR, paste0('Cannot set more that one value into single value field "', field, '".'))
		if (length(value) == 0)
			.self$message(MSG.ERROR, paste0('Cannot set an empty vector into single value field "', field, '".'))
	}

	# Check value class
	if (field.def$isVector())
		value <- as.vector(value, mode = field.def$getClass())

	.self$.fields[[field]] <- value
})

# Append field value {{{1
################################################################

BiodbEntry$methods(	appendFieldValue = function(field, value) {
	if (.self$hasField(field))
		.self$setFieldValue(field, c(.self$getFieldValue(field), value))
	else
		.self$setFieldValue(field, value)
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

# Remove field {{{1
################################################################

BiodbEntry$methods(	removeField = function(field) {
	if (.self$hasField(field))
		.fields <<- .self$.fields[names(.self$.fields) != field]
})

# Field has basic class {{{1
################################################################

BiodbEntry$methods(	fieldHasBasicClass = function(field) {
	return(.self$getBiodb()$getEntryFields()$getField(field)$getClass() %in% BIODB.BASIC.CLASSES)
})

# Get field value {{{1
################################################################

BiodbEntry$methods(	getFieldValue = function(field, compute = TRUE, flatten = FALSE) {

	val <- NULL

	# Check field
	.self$getBiodb()$getEntryFields()$checkIsDefined(field)

	# Compute field value
	if (compute && ! .self$hasField(field))
		.self$.compute.field(field)

	# Get value
	if (.self$hasField(field))
		val <- .self$.fields[[field]]
	else {
		# Return NULL or NA
		class = .self$getBiodb()$getEntryFields()$get(field)$getClass()
		val <- if (class %in% BIODB.BASIC.CLASSES) as.vector(NA, mode = class) else NULL
	}

	# Flatten: convert atomic values with cardinality > 1 into a string
	if (flatten)
		if (.self$getBiodb()$getEntryFields()$get(field)$isVector() && .self$getBiodb()$getEntryFields()$get(field)$hasCardOne())
			val <- paste(val, collapse = MULTIVAL.FIELD.SEP)

	return(val)
})

# Compute field {{{1
################################################################

BiodbEntry$methods(	.compute.field = function(fields = NULL) {

	success <- FALSE

	if (.self$getBiodb()$getConfig()$isEnabled(CFG.COMPUTE.FIELDS)) {

		# Set of fields to compute
		fields <- if (is.null(fields)) names(BIODB.FIELD.COMPUTING) else fields[fields %in% names(BIODB.FIELD.COMPUTING)]

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
BiodbEntry$methods(	getFieldsAsDataFrame = function(only.atomic = TRUE, compute = TRUE, fields = NULL) {
	"Convert entry into a data frame."

	df <- data.frame(stringsAsFactors = FALSE)

	# Compute fields
	if (compute)
		.self$.compute.field(fields)

	# Set fields to get
	fields <- if (is.null(fields)) names(.self$.fields) else fields[fields %in% names(.self$.fields)]

	# Loop on fields
	for (f in fields) {

		# Ignore non atomic values
		if (only.atomic && ! .self$getBiodb()$getEntryFields()$get(f)$isVector())
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

# Get fields as json {{{1
################################################################

# TODO add a limiting option to get some of the fields.
BiodbEntry$methods(	getFieldsAsJson = function(compute = TRUE) {
	"Convert entry into a JSON string."

	# Compute fields
	if (compute)
		.self$.compute.field()

	return(jsonlite::toJSON(.self$.fields, pretty = TRUE, digits = NA_integer_))
})

# Add Parsing expression {{{1
################################################################

BiodbEntry$methods( addParsingExpression = function(field, expr) {

	# Check that this field has no expression associated
	if (field %in% names(.self$.parsing.expr))
		.self$message(MSG.ERROR, paste("A parsing expression has already been defined for field", field))

	# Register new parsing expression
	.self$.parsing.expr[[field]] <- expr 
})

# Parse content {{{1
################################################################

BiodbEntry$methods( parseContent = function(content) {

	if (.self$.isContentCorrect(content)) {

		# Parse content
		parsed.content <- .self$.doParseContent(content)

		if (.self$.isParsedContentCorrect(parsed.content)) {

			.self$.parseFieldsFromExpr(parsed.content)

			.self$.parseFieldsAfter(parsed.content)
		}
	}
})

# Is content correct {{{1
################################################################

BiodbEntry$methods( .isContentCorrect = function(content) {
	return( ! is.null(content) && ! is.na(content) && nchar(content) > 0)
})

# Do parse content {{{1
################################################################

BiodbEntry$methods( .doParseContent = function(content) {
	.self$.abstract.method()
})

# Is parsed content correct {{{1
################################################################

BiodbEntry$methods( .isParsedContentCorrect = function(parsed.content) {
	return(TRUE)
})

# Parse fields from expressions {{{1
################################################################

BiodbEntry$methods( .parseFieldsFromExpr = function(parsed.content) {
	.self$.abstract.method()
})

# Parse fields after {{{1
################################################################

BiodbEntry$methods( .parseFieldsAfter = function(parsed.content) {
})

# DEPRECATED METHODS {{{1
################################################################

# Get Field {{{2
################################################################

BiodbEntry$methods(	getField = function(field) {
	.self$.deprecated.method("getFieldValue()")
	return(.self$getFieldValue(field))
})

# Set Field {{{2
################################################################

BiodbEntry$methods(	setField = function(field, value) {
	.self$.deprecated.method("setFieldValue()")
	.self$setFieldValue(field, value)
})

# Get field class {{{2
################################################################

BiodbEntry$methods(	getFieldClass = function(field) {

	.self$.deprecated.method('Biodb::getEntryFields()$get(field)$getClass()')

	return(.self$getBiodb()$getEntryFields()$get(field)$getClass())
})

# Get field cardinality {{{2
################################################################

BiodbEntry$methods(	getFieldCardinality = function(field) {

	.self$.deprecated.method('BiodbEntryFields::hasCardOne() or BiodbEntryFields::hasCardMany()')

	return(.self$getBiodb()$getEntryFields()$get(field)$getCardinality())
})
