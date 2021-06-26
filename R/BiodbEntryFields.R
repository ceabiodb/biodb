#' A class for handling description of all entry fields.
#'
#' The unique instance of this class is handle by the \code{\link{BiodbMain}}
#' class and accessed through the \code{getEntryFields()} method.
#'
#' @seealso \code{\link{BiodbMain}} and child class
#' \code{\link{BiodbEntryField}}.
#'
#' @examples
#' # Getting information about the accession field:
#' mybiodb <- biodb::newInst()
#' entry.field <- mybiodb$getEntryFields()$get('accession')
#'
#' # Test if a name is an alias of a field
#' mybiodb$getEntryFields()$isAlias('genesymbols')
#'
#' # Test if a name is associated with a defined field
#' mybiodb$getEntryFields()$isDefined('name')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @include BiodbMain.R
#' @include BiodbEntryField.R
#' @export BiodbEntryFields
#' @exportClass BiodbEntryFields
BiodbEntryFields <- methods::setRefClass("BiodbEntryFields",
    contains="BiodbObserver",
    fields=list( .fields="list",
        .aliasToName="character",
        .field.name.sep='character',
        .parent='ANY'
    ),
    methods=list(

initialize=function(parent) {

    .self$.parent <- parent

    .self$.fields <- list()
    .self$.aliasToName <- character(0)
    .self$.field.name.sep <- parent$getConfig()$get('intra.field.name.sep')
    parent$addObservers(.self)
},

cfgKVUpdate=function(k, v) {
    # Overrides BiodbObserver function.
    if (k == 'intra.field.name.sep')
        .self$.field.name.sep <- v
},

isAlias=function(name) {
    ":\n\nTests if names are aliases.
    \nname: A character vector of names or aliases to test.
    \nReturned value: A logical vector, the same length as `name`, with TRUE
    for name values that are an alias of a field, and FALSE otherwise."

    name <- .self$formatName(name)
    is_alias <- name %in% names(.self$.aliasToName)

    return(is_alias)
},

formatName=function(name) {
    ":\n\nFormat field name(s) for biodb format: set to lower case and remove
    dot or underscore characters depending on configuration.
    \nname: A character vector of names or aliases to test.
    \nReturned value: A character vector of formatted names.
    "

    name <- tolower(name)
    name <- gsub('[^a-z0-9]', .self$.field.name.sep, name)

    return(name)
},

isDefined=function(name) {
    ":\n\nTests if names are defined fields.
    \nname: A character vector of names or aliases to test.
    \nReturned value: A logical vector, the same length as `name`, with TRUE
    for name values that corresponds to a defined field.
    "

    name <- .self$formatName(name)

    return(name %in% names(.self$.fields) | .self$isAlias(name))
},

checkIsDefined=function(name) {
    ":\n\nTests if names are valid defined fields. Throws an error if any name
    does not correspond to a defined field.
    \nname: A character vector of names or aliases to test.
    \nReturned value: None.
    "

    def <- .self$isDefined(name)
    if (any( ! def))
        error0("Field(s) \"", paste(name[ ! def], collapse=", "),
            "\" is/are not defined.")
},

getRealName=function(name, fail=TRUE) {
    ":\n\nGets the real names (main names) of fields. If some name is not
    found neither in aliases nor in real names, an error is thrown.
    \nname: A character vector of names or aliases.
    \nfail: Fails if name is unknown.
    \nReturned value: A character vector, the same length as `name`, with the
    real field name for each name given (i.e.: each alias is replaced with the
    real name).
    "
    chk::chk_character(name)
    chk::chk_flag(fail)

    name <- .self$formatName(name)

    # Check name
    if (fail)
        .self$checkIsDefined(name)

    # Get real name
    isNotRealName <-  ! name %in% names(.self$.fields) 
    if (any(isNotRealName)) {
        realName <- .self$.aliasToName[name[isNotRealName]]
        if (any(is.na(realName)) && fail)
            error("Unknown fields: %s.",
                paste(name[isNotRealName][is.na(realName)], collapse=', '))
        name[isNotRealName][ ! is.na(realName)] <- realName[ ! is.na(realName)]
    }

    return(name)
},

get=function(name, drop=TRUE) {
    ":\n\nGets a BiodbEntryField instance.
    \nname: A character vector of names or aliases.
    \ndrop: If TRUE and only one name has been submitted, returns a single
    BiodbEntryField instance instead of a list.
    \nReturned value: A named list of BiodbEntryField instances. The names of
    the list are the real names of the entry fields, thus they may be different
    from the one provided inside the name argument.
    "
    chk::chk_character(name)
    chk::chk_flag(drop)

    biodb::logTrace('Asked field names are: %s.', paste(name, collapse=', '))
    name <- .self$getRealName(name)
    biodb::logTrace('Realnames of fields are: %s.', paste(name, collapse=', '))
    fields <- .self$.fields[tolower(name)]
    biodb::logTrace('%d fields were returned.', length(fields))
    biodb::logTrace('fields variable is a %s.', class(fields))
    if (drop && length(fields) == 1)
        fields <- fields[[1]]
    biodb::logTrace('END OF BiodbEntryFields::get()')

    return(fields)
},

getFieldNames=function(type=NULL, computable=NULL) {
    ":\n\nGets the main names of all fields.
    \ntype: Set this parameter to a character vector in order to
    return only the names of the fields corresponding to the types
    specified.
    \ncomputable: If set to TRUE, returns only the names of
    computable fields. If set to FALSE, returns only the names of
    fields that are not computable.
    \nReturned value: A character vector containing all selected field names.
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

    # Filter on computability
    if ( ! is.null(computable)) {
        fct <- function(f) {
            .self$.fields[[f]]$isComputable()
        }
        fields <- Filter(fct, fields)
    }

    return(sort(fields))
},

getDatabaseIdField=function(database) {
    ":\n\nGets a database ID field.
    \ndatabase: The name (i.e.: Biodb ID) of a database.
    \nReturned value: The name of the field handling identifiers (i.e.:
    accession numbers) for this database.
    "

    dbs <- .self$.parent$getDbsInfo()
    return(.self$get(dbs$get(database)$getIdFieldName()))
},

show=function() {
    ":\n\nPrints information about the instance.
    \nReturned value: None.
    "

    cat("Biodb entry fields information instance.\n")
},

define=function(def) {
    ":\n\nDefines fields.
    \ndef: A named list of field definitions. The names of the list are the
    main names of the fields.
    \nReturned value: None.
    "

    # Loop on all fields
    for (f in names(def)) {
        logDebug("Define field %s.", f)
        args <- def[[f]]
        args[['name']] <- f
        do.call(.self$.defineField, args)
    }
},

.defineField=function(name, ...) {

    name <- .self$formatName(name)

    # Create new field instance
    field <- BiodbEntryField$new(parent=.self, name=name, ...)

    # Is field already defined?
    if (.self$isDefined(name)) {
        .self$.fields[[name]]$equals(field, fail=TRUE)
        .self$.fields[[name]]$updateWithValuesFrom(field)
    }

    # Register new field
    else {
        # Register new field inside fields list
        .self$.fields[[name]] <- field

        # Define aliases
        if (field$hasAliases())
            for (alias in field$getAliases())
                .self$.aliasToName[[alias]] <- name
    }
    
    # Check
    .self$.fields[[name]]$.check()
}

))
