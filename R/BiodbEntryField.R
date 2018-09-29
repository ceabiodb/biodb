# vi: fdm=marker

# Constants {{{1
################################################################

# Cardinalities
BIODB.CARD.ONE <- '1'
BIODB.CARD.MANY <- '*'
FIELD.CARDINALITIES <- c(BIODB.CARD.ONE, BIODB.CARD.MANY)

FIELD.CLASSES <- c('character', 'integer', 'double', 'logical', 'object', 'data.frame')

# Class declaration {{{1
################################################################

#' A class for describing an entry field.
#'
#' This class is used by \code{\link{BiodbEntryFields}} for storing field characteristics, and returning them through the \code{get()} method. The constructor is not meant to be used, but for development purposes the constructor's parameters are nevertheless described in the Fields section.
#'
#' @field db.id             Set to \code{TRUE} if the field is a database ID.
#' @field name              The name of the field.
#' @field alias             A character vector containing zero or more aliases for the field.
#' @field class             The class of the field. One of: 'character', 'integer', 'double', 'logical', 'object', 'data.frame'.
#' @field card              The cardinality of the field: either '1' or '*'.
#' @field allow.duplicates  If set to \code{TRUE}, the field allows duplicated values.
#' @field description       The description of the field.
#' @field allowed.values    The values authorized for the field.
#'
#' @seealso \code{\link{BiodbEntryFields}}.
#'
#' @examples
#' # Get the class of the InChI field.
#' mybiodb <- biodb::Biodb()
#' inchi.field.class <- mybiodb$getEntryFields()$get('inchi')$getClass()
#'
#' # Test the cardinality of a field
#' card.one <- mybiodb$getEntryFields()$get('name')$hasCardOne()
#' card.many <- mybiodb$getEntryFields()$get('name')$hasCardMany()
#'
#' # Get the description of a field
#' desc <- mybiodb$getEntryFields()$get('inchi')$getDescription()
#'
#' @import methods
#' @include biodb-common.R
#' @include ChildObject.R
#' @export BiodbEntryField
#' @exportClass BiodbEntryField
BiodbEntryField <- methods::setRefClass("BiodbEntryField", contains = "ChildObject", fields = list( .name = 'character', .type = 'character', .class = 'character', .cardinality = 'character', .forbids.duplicates = 'logical', .db.id = 'logical', .description = 'character', .alias = 'character', .allowed.values = "ANY", .lower.case = 'logical', .case.insensitive = 'logical'))

# Constructor {{{1
################################################################

BiodbEntryField$methods( initialize = function(name, alias = NA_character_, type = NA_character_, class = 'character', card = BIODB.CARD.ONE, forbids.duplicates = FALSE, db.id = FALSE, description = NA_character_, allowed.values = NULL, lower.case = FALSE, case.insensitive = FALSE, ...) {

	callSuper(...)

	# Set name
	if ( is.null(name) || is.na(name) || nchar(name) == '')
		.self$message('error', "You cannot set an empty name for a field. Name was empty (either NULL or NA or empty string).")
	.name <<- tolower(name)

	# Set type
	if ( ! is.na(type) && ! type %in% c('mass', 'name'))
		.self$message('error', paste("Unknown type \"", type, "\" for field \"", name, "\".", sep = ''))
	.type <<- type

	# Set class
	if ( ! class %in% FIELD.CLASSES)
		.self$message('error', paste("Unknown class \"", class, "\" for field \"", name, "\".", sep = ''))
	.class <<- class

	# Set cardinality
	if ( ! card %in% FIELD.CARDINALITIES)
		.self$message('error', paste("Unknown cardinality \"", card, "\" for field \"", name, "\".", sep = ''))
	.cardinality <<- card

	# Set description
	if (is.null(description) || is.na(description))
		.self$message('caution', paste("Missing description for entry field \"", name, "\".", sep = ''))
	.description <<- description

	# Set alias
	if (length(alias) > 1 && any(is.na(alias)))
		.self$message('error', paste("One of the aliases of entry field \"", name, "\" is NA.", sep = ''))
	.alias <<- alias

	# Set allowed values
	if ( ! is.null(allowed.values)) {
		if ( ! is.vector(allowed.values, mode = 'numeric') && ! is.vector(allowed.values, mode = 'character') && ! is.vector(allowed.values, mode = 'list'))
			.self$message('error', 'Allowed values must be either a list, a numeric vector or a character vector.')

		# For a list check that all values are character vectors
		if (is.vector(allowed.values, mode = 'list')) {
			if (is.null(names(allowed.values)))
				.self$message('error', 'When allowed values are specified as a list, names must be set.')
			if ( ! all(vapply(allowed.values, function(x) is.vector(x, 'character'), FUN.VALUE = TRUE)))
				.self$message('error', 'When allowed values are specified as a list, all values must be characters.')
		}
	}
	.allowed.values <<- allowed.values

	# Case insensitive
	if (case.insensitive && class != 'character')
		.self$message('error', 'Only character fields can be case insensitive.')
	.case.insensitive <<- case.insensitive

	# Lower case
	if (lower.case && class != 'character')
		.self$message('error', 'Only character fields can be forced to lower case.')
	.lower.case <<- lower.case

	# Set other fields
	.forbids.duplicates <<- forbids.duplicates
	.db.id <<- db.id

})

# Get name {{{1
################################################################

BiodbEntryField$methods( getName = function() {
	":\n\n Get field's name."

	return(.self$.name)
})

# Get type {{{1
################################################################

BiodbEntryField$methods( getType = function() {
	":\n\n Get field's type."

	return(.self$.type)
})

# Get description {{{1
################################################################

BiodbEntryField$methods( getDescription = function() {
	":\n\n Get field's description."

	return(.self$.description)
})

# Has aliases {{{1
################################################################

BiodbEntryField$methods( hasAliases = function() {
	":\n\n Returns TRUE if this entry field defines aliases."

	return( ! any(is.na(.self$.alias)))
})

# Get aliases {{{1
################################################################

BiodbEntryField$methods( getAliases = function() {
	":\n\n Returns the list of aliases if some are defined, otherwise returns NULL."

	aliases <- NULL

	if (.self$hasAliases())
		aliases <- .self$.alias
	
	return(aliases)
})

# Get all names {{{1
################################################################

BiodbEntryField$methods( getAllNames = function() {
	":\n\n Returns the list of all names (main name and aliases)."

	aliases <- .self$getAliases()
	names <- .self$getName()
	if ( ! is.null(aliases))
		names <- c(names, aliases)
	
	return(names)
})

# Is enumerated {{{2
################################################################

BiodbEntryField$methods( isEnumerated = function() {
	return( ! is.null(.self$.allowed.values))
})

# Correct value {{{1
################################################################

BiodbEntryField$methods( correctValue = function(value) {

	# Correct type
	if (.self$isVector() && .self$getClass() != class(value))
		value <- as.vector(value, mode = .self$getClass())

	# Lower case
	if (.self$.lower.case)
		value <- tolower(value)

	# Enumerated type
	if (.self$isEnumerated() && class(.self$.allowed.values) == 'list')
		value <- vapply(value, function(v) { match <- vapply(.self$.allowed.values, function(a) v %in% a, FUN.VALUE = TRUE) ; if (any(match)) names(.self$.allowed.values)[match] else v }, FUN.VALUE = as.vector(0, mode = .self$getClass()))

	return(value)
})

# Is enumerate {{{1
################################################################

BiodbEntryField$methods( isEnumerate = function() {
	return( ! is.null(.self$.allowed.values))
})

# Get allowed values {{{1
################################################################

BiodbEntryField$methods( getAllowedValues = function() {

	values <- NULL
	if ( ! is.null(.self$.allowed.values)) {
		values <- unlist(.self$.allowed.values)
		if ( ! is.null(names(.self$.allowed.values)))
			values <- c(values, names(.self$.allowed.values))
		names(values) <- NULL
	}

	return(values)
})

# Check value {{{1
################################################################

BiodbEntryField$methods( checkValue = function(value) {

	if (.self$.lower.case)
		value <- tolower(value)

	bad.values <- value[ ! value %in% .self$getAllowedValues()]
	if (.self$isEnumerated() && length(bad.values) > 0)
		.self$message('error', paste('Value(s) ', paste(bad.values[ ! duplicated(bad.values)], collapse = ', '), ' is/are not allowed for field ', .self$getName(), '. Allowed values are: ', paste(.self$getAllowedValues(), collapse = ', '), '.', sep = ''))
})

# Has card one {{{1
################################################################

BiodbEntryField$methods( hasCardOne = function() {
	":\n\n Returns \\code{TRUE} if the cardinality of this field is one."

	return(.self$.cardinality == BIODB.CARD.ONE)
})

# Has card many {{{1
################################################################

BiodbEntryField$methods( hasCardMany = function() {
	":\n\n Returns \\code{TRUE} if the cardinality of this field is many."

	return(.self$.cardinality == BIODB.CARD.MANY)
})

# Forbids duplicates {{{1
################################################################

BiodbEntryField$methods( forbidsDuplicates = function() {
	":\n\n Returns \\code{TRUE} if this field forbids duplicated values."

	return(.self$.forbids.duplicates)
})

# Is case insensitive {{{1
################################################################

BiodbEntryField$methods( isCaseInsensitive = function() {
	":\n\n Returns \\code{TRUE} if this field is case insensitive."

	return(.self$.case.insensitive)
})

# Get class {{{1
################################################################

BiodbEntryField$methods( getClass = function() {
	":\n\n Returns the type (i.e.: class) of this field."

	return(.self$.class)
})

# Is object {{{1
################################################################

BiodbEntryField$methods( isObject = function() {
	":\n\n Returns \\code{TRUE} if field's type is a class."

	return(.self$.class == 'object')
})

# Is data frame {{{1
################################################################

BiodbEntryField$methods( isDataFrame = function() {
	":\n\n Returns \\code{TRUE} if field's type is data frame."

	return(.self$.class == 'data.frame')
})

# Is vector {{{1
################################################################

BiodbEntryField$methods( isVector = function() {
	":\n\nReturns \\code{TRUE} if the field's type is vector (i.e.: character, integer, double or logical)."
	return(.self$.class %in% c('character', 'integer', 'double', 'logical'))
})

# DEPRECATED METHODS {{{1
################################################################

# Get cardinality {{{2
################################################################

BiodbEntryField$methods( getCardinality = function() {
	.self$.deprecated.method('hasCardOne() or hasCardMany()')
	return(.self$.cardinality)
})
