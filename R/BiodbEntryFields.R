# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbEntryFields {{{1
################################################################################

# Declaration {{{2
################################################################################

#' A class for handling description of all entry fields.
#'
#' The unique instance of this class is handle by the \code{\link{Biodb}} class
#' and accessed through the \code{getEntryFields()} method.
#'
#' @seealso \code{\link{Biodb}} and child class \code{\link{BiodbEntryField}}.
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
    contains="BiodbChildObject",
    fields=list( .fields="list",
                  .aliasToName="character"),
    methods=list(

# Public methods {{{2
################################################################################

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)

    .self$.fields <- list()
    .self$.aliasToName <- character(0)
},

# Is alias {{{3
################################################################################

isAlias=function(name) {
    ":\n\nTests if names are aliases.
    \nname: A character vector of names or aliases to test.
    \nReturned value: A logical vector, the same length as `name`, with TRUE
    for name values that are an alias of a field, and FALSE otherwise."

    return(tolower(name) %in% names(.self$.aliasToName))
},

# Is defined {{{3
################################################################################

isDefined=function(name) {
    ":\n\nTests if names are defined fields.
    \nname: A character vector of names or aliases to test.
    \nReturned value: A logical vector, the same length as `name`, with TRUE
    for name values that corresponds to a defined field.
    "
    
    return(tolower(name) %in% names(.self$.fields) | .self$isAlias(name))
},

# Check is defined {{{3
################################################################################

checkIsDefined=function(name) {
    ":\n\nTests if names are valid defined fields. Throws an error if any name
    does not correspond to a defined field.
    \nname: A character vector of names or aliases to test.
    \nReturned value: None.
    "

    def <- .self$isDefined(name)
    if (any( ! def))
        .self$error("Field(s) \"", paste(name[ ! def], collapse=", "),
                    "\" is/are not defined.")
},

# Get real name {{{3
################################################################################

getRealName=function(name) {
    ":\n\nGets the real names (main names) of fields. If some name is not
    found neither in aliases nor in real names, an error is thrown.
    \nname: A character vector of names or aliases.
    \nReturned value: A character vector, the same length as `name`, with the
    real field name for each name given (i.e.: each alias is replaced with the
    real name).
    "

    .self$checkIsDefined(name)

    if ( ! tolower(name) %in% names(.self$.fields))
        name <- .self$.aliasToName[[tolower(name)]]

    return(name)
},

# Get {{{3
################################################################################

get=function(name) {
    ":\n\nGets a BiodbEntryField instance.
    \nname: A character vector of names or aliases.
    \nReturned value: The BiodbEntryField instance associated with `name`.
    "

    name <- .self$getRealName(name)
    field <- .self$.fields[[tolower(name)]]
    return(field)
},

# Get field names {{{3
################################################################################

getFieldNames=function(type=NULL) {
    ":\n\nGets the main names of all fields.
    \ntype: If set, returns only the field names corresponding to this type.
    \nReturned value: A character vector containing all field names.
    "

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
},

# Get database id field {{{3
################################################################################

getDatabaseIdField=function(database) {
    ":\n\nGets a database ID field.
    \ndatabase: The name (i.e.: Biodb ID) of a database.
    \nReturned value: The name of the field handling identifiers (i.e.:
    accession numbers) for this database.
    "

    dbs <- .self$getBiodb()$getDbsInfo()
    return(.self$get(dbs$get(database)$getIdFieldName()))
},

# Show {{{3
################################################################################

show=function() {
    ":\n\nPrints information about the instance.
    \nReturned value: None.
    "

    cat("Biodb entry fields information instance.\n")
},

# Define {{{3
################################################################################

define=function(def) {
    ":\n\nDefines fields.
    \ndef: A named list of field definitions. The names of the list are the
    main names of the fields.
    \nReturned value: None.
    "

    # Loop on all fields
    for (f in names(def)) {
        args <- def[[f]]
        args[['name']] <- f
        do.call(.self$.defineField, args)
    }
},

# Private methods {{{2
################################################################################

# Define field {{{3
################################################################################

.defineField=function(name, ...) {

    # Make sure name is in lower case
    name <- tolower(name)

    # Create new field instance
    field <- BiodbEntryField$new(parent=.self, name=name, ...)

    # Is field already defined?
    defined <- FALSE
    if (.self$isDefined(name)) {
        defined <- TRUE
        if ( ! .self$.fields[[name]]$equals(field))
            .self$error("Field \"", name, "\" has already been defined.")
    }

    if ( ! defined) {
        # Register new field inside fields list
        .self$.fields[[name]] <- field

        # Define aliases
        if (field$hasAliases())
            for (alias in field$getAliases())
                .self$.aliasToName[[alias]] <- name
    }
}

))
