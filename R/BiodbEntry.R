# vi: fdm=marker

# Entry abstract class {{{1
################################################################

#' The mother abstract class of all database entry classes.
#'
#' An entry is an element of a database, identifiable by its accession number. Each contains a list of fields defined by a name and a value. The details of all fields that can be set into an entry are defined inside the class \code{BiodbEntryFields}. From this class are derived other abstract classes for different types of entry contents: \code{TxtEntry}, \code{XmlEntry}, \code{CsvEntry}, \code{JsonEntry} and \code{HtmlEntry}. Then concrete classes are derived for each database: \code{ChebiEntry}, \code{ChemspiderEntru}, etc. For biodb users, there is no need to know this hierarchy; the knowledge of this class and its methods is sufficient.
#'
#' @param compute       If set to \code{TRUE} and a field is not defined, try to compute it using internal defined computing rules. If set to \code{FALSE}, let the field undefined.
#' @param content       A character string containing definition for an entry and obtained from a database. The format can be: CSV, HTML, JSON, XML, or just text.
#' @param field         The nane of a field.
#' @param fields        Set to character vector of field names in order to restrict execution to this set of fields.
#' @param flatten       If set to \code{TRUE} and a field's value is a vector of more than one element, then export the field's value as a single string composed of the field's value concatenated and separated by the character defined in the 'multival.field.sep' config key. If set to \code{FALSE} or the field contains only one value, changes nothing.
#' @param last          If set to \code{TRUE} and a field's value is a vector of more than one element, then export only the last value. If set to \code{FALSE}, changes nothing.
#' @param only.atomic   If set to \code{TRUE}, only export field's values that are atomic (i.e.: of type vector and length one).
#' @param parsing.expr  A parsing expression used to parse entry content string and obtain a field's value.
#' @param value         A field's value.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{BiodbConn}}, \code{\link{BiodbEntryFields}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get an entry:
#' entry <- mybiodb$getFactory()$getEntry('chebi', '1')
#'
#' # Get all defined fields:
#' entry$getFieldNames()
#'
#' # Get a field value:
#' smiles <- entry$getFieldValue('smiles')
#'
#' # Test if a field is defined:
#' if (entry$hasField('charge'))
#'   print(paste('The entry has a charge of ', entry$getFieldValue('charge'), '.', sep = ''))
#'
#' # Export an entry as a data frame:
#' df <- entry$getFieldsAsDataFrame()
#'
#' # Even if you may not do it, you can set a field's value yourselves:
#' entry$setFieldValue('mass', 1893.1883)
#'
#' # Or even add a new field:
#' entry$setFieldValue('chemspider.id', '388394')
#'
#' @import methods
#' @include ChildObject.R
#' @export BiodbEntry
#' @exportClass BiodbEntry
BiodbEntry <- methods::setRefClass("BiodbEntry", contains = "ChildObject", fields = list(.fields ='list', .parsing.expr = 'ANY'))

# Constructor {{{1
################################################################

BiodbEntry$methods( initialize = function(...) {

	callSuper(...)
	.self$.abstract.class('BiodbEntry')

	.fields <<- list()
	.parsing.expr <<- list()
})

# Set field value {{{1
################################################################

BiodbEntry$methods(	setFieldValue = function(field, value) {
	":\n\nSet the value of a field. If the field is not already set for this entry, then the field will be created. See BiodbEntryFields for a list of possible fields in biodb."

	field.def <- .self$getBiodb()$getEntryFields()$get(field)
	field <- field.def$getName()

	# Remove duplicates
	if ( ! field.def$allowsDuplicates() && is.vector(value))
		value <- value[ ! duplicated(value)]

	# Specific case to handle objects.
	if (field.def$isObject() && !(isS4(value) & methods::is(value, "refClass")))
	  .self$message('error', paste0('Cannot set a non RC instance to field "', field, '" in BiodEntry.'))
	
	# Check cardinality
	if ( ! field.def$isDataFrame() && field.def$hasCardOne()) {
		if (length(value) > 1)
			.self$message('error', paste0('Cannot set more that one value (', paste(value, collapse = ', '), ') into single value field "', field, '".'))
		if (length(value) == 0)
			.self$message('error', paste0('Cannot set an empty vector into single value field "', field, '".'))
	}

	# Check value class
	if (field.def$isVector()) {
		v <- as.vector(value, mode = field.def$getClass())
		if ( ! is.na(value) && is.na(v))
			.self$message('caution', paste("Unable to convert value \"", value, "\" into ", field.def$getClass(), " type for field \"", field, "\".", sep = ''))
		value <- v
	}

	# Check value
	field.def$checkValue(value)

	# Correct value
	value <- field.def$correctValue(value)

	# Set value
	.self$.fields[[field.def$getName()]] <- value
})

# Append field value {{{1
################################################################

BiodbEntry$methods(	appendFieldValue = function(field, value) {
	":\n\nAppend a value to an existing field. If the field is not defined for this entry, then the field will be created and set to this value. Only fields with a cardinality greater than one can accept multiple values."

	if (.self$hasField(field))
		.self$setFieldValue(field, c(.self$getFieldValue(field), value))
	else
		.self$setFieldValue(field, value)
})

# Get field names {{{1
################################################################

BiodbEntry$methods(	getFieldNames = function() {
	":\n\nGet a list of all fields defined for this entry."

	return(names(.self$.fields))
})

# Has field {{{1
################################################################

BiodbEntry$methods(	hasField = function(field) {
	":\n\nReturns TRUE if the specified field is defined in this entry."

	# Get field definition
	field.def <- .self$getBiodb()$getEntryFields()$get(field)
	field <- field.def$getName()

	return(tolower(field) %in% names(.self$.fields))
})

# Remove field {{{1
################################################################

BiodbEntry$methods(	removeField = function(field) {
	":\n\nRemove the specified field from this entry."

	if (.self$hasField(field))
		.fields <<- .self$.fields[names(.self$.fields) != tolower(field)]
})

# Get field value {{{1
################################################################

BiodbEntry$methods(	getFieldValue = function(field, compute = TRUE, flatten = FALSE, last = FALSE) {
	":\n\nGet the value of the specified field."

	val <- NULL
	field <- tolower(field)

	# Get field definition
	field.def <- .self$getBiodb()$getEntryFields()$get(field)
	field <- field.def$getName()

	# Compute field value
	if (compute && ! .self$hasField(field))
		.self$computeFields(field)

	# Get value
	if (.self$hasField(field))
		val <- .self$.fields[[field]]
	else
		# Return NULL or NA
		val <- if (field.def$isVector()) as.vector(NA, mode = field.def$getClass()) else NULL

	# Get last value only
	if (last && field.def$hasCardMany() && length(val) > 1)
		val <- val[[length(val)]]

	# Flatten: convert atomic values with cardinality > 1 into a string
	if (flatten && ! is.null(val)) {
		if (field.def$isVector() && field.def$hasCardMany() && length(val) > 1) {
			if (all(is.na(val)))
				val <-  as.vector(NA, mode = field.def$getClass())
			else
				val <- paste(val, collapse = .self$getBiodb()$getConfig()$get('multival.field.sep'))
		}
	}

	return(val)
})

# Get fields as data frame {{{1
################################################################

BiodbEntry$methods(	getFieldsAsDataFrame = function(only.atomic = TRUE, compute = TRUE, fields = NULL) {
	":\n\nConvert this entry into a data frame."

	df <- data.frame(stringsAsFactors = FALSE)
	if ( ! is.null(fields))
		fields <- tolower(fields)

	# Compute fields
	if (compute)
		.self$computeFields(fields)

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

BiodbEntry$methods(	getFieldsAsJson = function(compute = TRUE) {
	":\n\nConvert entry into a JSON string."

	# Compute fields
	if (compute)
		.self$computeFields()

	return(jsonlite::toJSON(.self$.fields, pretty = TRUE, digits = NA_integer_))
})

# Add Parsing expression {{{1
################################################################

BiodbEntry$methods( addParsingExpression = function(field, parsing.expr) {
	":\n\nAdd a parsing expression for the specified field. The form of the parsing expression depends on the type of content and thus of the class inherited from BiodbEntry abstract class: CsvEntry, TxtEntry, XmlEntry, etc. This method is automatically called internally and should not be called by the user."

	field <- tolower(field)

	# Check that this field has no expression associated
	if (field %in% names(.self$.parsing.expr))
		.self$message('error', paste("A parsing expression has already been defined for field", field))

	# Register new parsing expression
	.self$.parsing.expr[[field]] <- parsing.expr 
})

# Get parsing expressions {{{1
################################################################

BiodbEntry$methods( getParsingExpressions = function() {
	":\n\nReturn a list of all defined parsing expressions for this entry."

	return(.self$.parsing.expr)
})

# Parse content {{{1
################################################################

BiodbEntry$methods( parseContent = function(content) {
	":\n\nParse content string and set values accordingly for this entry's fields. This method is called automatically and should be run directly by users."

	if (.self$.isContentCorrect(content)) {

		# Parse content
		parsed.content <- .self$.doParseContent(content)

		if (.self$.isParsedContentCorrect(parsed.content)) {

			.self$.parseFieldsFromExpr(parsed.content)

			.self$.parseFieldsAfter(parsed.content)
		}
	}

	# Make sure the database id field is set to the same value as the accession field
	dbid.field <- .self$getParent()$getDbInfo()$getEntryIdField()
	if (.self$hasField(dbid.field) && .self$hasField('accession')) {
		if (.self$getFieldValue('accession') != .self$getFieldValue(dbid.field))
			.self$message('error', paste('Value of accession field ("', .self$getFieldValue('accession'), '") is different from value of ', dbid.field, ' field ("', .self$getFieldValue(dbid.field), '").', sep = ''))
	}
	else {
		if (.self$hasField(dbid.field))
			.self$setFieldValue('accession', .self$getFieldValue(dbid.field))
		else
			.self$setFieldValue(dbid.field, .self$getFieldValue('accession'))
	}
})

# Compute fields {{{1
################################################################

BiodbEntry$methods(	computeFields = function(fields = NULL) {

	success <- FALSE
	if ( ! is.null(fields))
		fields <- tolower(fields)

	if (.self$getBiodb()$getConfig()$isEnabled('compute.fields')) {

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
					.self$message('debug', paste("Compute value for field \"", f, "\".", sep = '')) 
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

# PRIVATE METHODS {{{1
################################################################

# Is content correct {{{2
################################################################

BiodbEntry$methods( .isContentCorrect = function(content) {

	correct <- ! is.null(content) && ! is.na(content) && content != ''
	# NOTE `nchar(content)` may give "invalid multibyte string, element 1" on some strings.

	return(correct)
})

# Do parse content {{{2
################################################################

BiodbEntry$methods( .doParseContent = function(content) {
	.self$.abstract.method()
})

# Is parsed content correct {{{2
################################################################

BiodbEntry$methods( .isParsedContentCorrect = function(parsed.content) {
	return(TRUE)
})

# Parse fields from expressions {{{2
################################################################

BiodbEntry$methods( .parseFieldsFromExpr = function(parsed.content) {
	.self$.abstract.method()
})

# Parse fields after {{{2
################################################################

BiodbEntry$methods( .parseFieldsAfter = function(parsed.content) {
})

# Show {{{1
################################################################

BiodbEntry$methods( show = function() {
	cat("Biodb", .self$getParent()$getDbInfo()$getName(), "entry instance.\n")
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

	.self$.deprecated.method('BiodbEntryField::hasCardOne() or BiodbEntryField::hasCardMany()')

	return(.self$getBiodb()$getEntryFields()$get(field)$getCardinality())
})

# Field has basic class {{{2
################################################################

BiodbEntry$methods(	fieldHasBasicClass = function(field) {

	.self$.deprecated.method('BiodbEntryField::isVector()')

	return(.self$getBiodb()$getEntryFields()$get(field)$isVector())
})

# Compute field {{{2
################################################################

BiodbEntry$methods(	.compute.field = function(fields = NULL) {

	.self$.deprecated.method('BiodbEntry::computeFields()')

	return(.self$computeFields(fields = fields))
})
