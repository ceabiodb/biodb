# vi: fdm=marker ts=4 et cc=80

# BiodbEntryFields {{{1
################################################################

#' A class for handling description of all entry fields.
#'
#' The unique instance of this class is handle by the \code{\link{Biodb}} class and accessed through the \code{getEntryFields()} method.
#'
#' @seealso \code{\link{Biodb}}, \code{\link{BiodbEntryField}}.
#'
#' @param name      The name or alias of a field.
#' @param database  The name of a database.
#'
#' @examples
#' # Getting information about the accession field:
#' mybiodb <- biodb::Biodb()
#' entry.field <- mybiodb$getEntryFields()$get('accession')
#'
#' # Test if a name is an alias of a field
#' mybiodb$getEntryFields()$isAlias('genesymbols')
#'
#' # Test if a name is associated with a defined field
#' mybiodb$getEntryFields()$isDefined('chebi.id')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @include Biodb.R
#' @include BiodbChildObject.R
#' @include BiodbEntryField.R
#' @export BiodbEntryFields
#' @exportClass BiodbEntryFields
BiodbEntryFields <- methods::setRefClass("BiodbEntryFields",
    contains = "BiodbChildObject",
    fields = list( .fields = "list",
                  .aliasToName = "character"),
    methods = list(

# Public methods {{{2
################################################################################

# Define {{{3
############################################################lipidmaps.structure
define = function(def) {

	# Loop on all fields
	for (f in names(def)) {
		args <- def[[f]]
		args[['name']] <- f
		do.call(.self$.defineField, args)
	}
},

# Is defined {{{3
################################################################

isDefined = function(name) {
	":\n\nReturns TRUE if name corresponds to a defined field."
	return(tolower(name) %in% names(.self$.fields) || .self$isAlias(name))
},

# Private methods {{{2
################################################################

# Define field {{{3
################################################################

.defineField = function(name, ...) {

	# Make sure name is in lower case
	name <- tolower(name)

	# Is field already defined?
	if (.self$isDefined(name))
		.self$message('error', paste("Field \"", name, "\" has already been defined.", sep = ''))

	# Define new field
	field <- BiodbEntryField$new(parent = .self, name = name, ...)

	# Store inside fields list
	.self$.fields[[name]] <- field

	# Define aliases
	if (field$hasAliases())
		for (alias in field$getAliases())
			.self$.aliasToName[[alias]] <- name
}

))

# Constructor {{{1
################################################################

BiodbEntryFields$methods( initialize = function(...) {

	callSuper(...)

	.self$.fields <- list()
	.self$.aliasToName <- character(0)
})

# Is alias {{{1
################################################################

BiodbEntryFields$methods( isAlias = function(name) {
	":\n\nReturns TRUE if name is an alias of a field."

	return(tolower(name) %in% names(.self$.aliasToName))
})

# Check is defined {{{1
################################################################

BiodbEntryFields$methods( checkIsDefined = function(name) {
	":\n\nThrows an error if name does not correspond to a defined field."

	if ( ! .self$isDefined(name))
		.self$message('error', paste("Field \"", name, "\" is not defined.", sep = ''))
})

# Get real name {{{1
################################################################

BiodbEntryFields$methods( getRealName = function(name) {
	":\n\nIf name is an alias, returns the main name of the field. If name is not found neither in aliases nor in real names, an error is thrown."

	.self$checkIsDefined(name)

	if ( ! tolower(name) %in% names(.self$.fields))
		name <- .self$.aliasToName[[tolower(name)]]

	return(name)
})

# Get {{{1
################################################################

BiodbEntryFields$methods( get = function(name) {
	":\n\nReturns the BiodbEntryField instance associated with name."

	name <- .self$getRealName(name)
	field <- .self$.fields[[tolower(name)]]
	return(field)
})

# Get field names {{{1
################################################################

BiodbEntryFields$methods( getFieldNames = function(type = NULL) {
	":\n\nReturns the main names of all fields."

	# Filter by type
	if ( ! is.null(type)) {
		fields <- character()
		for (n in names(.self$.fields))
			if (.self$.fields[[n]]$getType() %in% type)
				fields <- c(fields, n)
	}

	else
		fields <- names(.self$.fields)

	return(sort(fields))
})

# Get database id field {{{1
################################################################

BiodbEntryFields$methods( getDatabaseIdField = function(database) {
	":\n\nReturns the name of the field handling identifiers (i.e.: accession numbers) for this database."

	return(.self$get(.self$getBiodb()$getDbsInfo()$get(database)$getIdFieldName()))
})

# Show {{{1
################################################################

BiodbEntryFields$methods( show = function() {
	cat("Biodb entry fields information instance.\n")
})

# Load fields file {{{1
################################################################

BiodbEntryFields$methods( loadFieldsFile = function(file) {
	'Load entry fields information file, and defines the new fields.
	The parameter file must point to a valid YAML file.'

	fields <- yaml::read_yaml(file)
})

