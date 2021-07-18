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
#' @import R6
#' @include BiodbMain.R
#' @include BiodbEntryField.R
#' @export
BiodbEntryFields <- R6::R6Class("BiodbEntryFields",

public=list(

#' @description
#' New instance initializer. No BiodbEntryFields instance must be created
#' directly. Instead, call the getEntryFields() method of BiodbMain.
#' @param parent The BiodbMain instance.
#' @return Nothing.
initialize=function(parent) {

    private$parent <- parent

    private$fields <- list()
    private$aliasToName <- character(0)
    private$field.name.sep <- parent$getConfig()$get('intra.field.name.sep')
    parent$addObservers(self)

    return(invisible(NULL))
},

#' @description
#' Call back method called when a value is modified inside the configuration.
#' @param k The config key name.
#' @param v The value associated with the key.
#' @return Nothing.
notifyCfgUpdate=function(k, v) {
    if (k == 'intra.field.name.sep')
        private$field.name.sep <- v

    return(invisible(NULL))
},

#' @description
#' Tests if names are aliases.
#' @param name A character vector of names or aliases to test.
#' @return A logical vector, the same length as `name`, with TRUE
#'     for name values that are an alias of a field, and FALSE otherwise."
isAlias=function(name) {
    name <- self$formatName(name)
    is_alias <- name %in% names(private$aliasToName)

    return(is_alias)
},

#' @description
#' Format field name(s) for biodb format: set to lower case and remove
#'     dot or underscore characters depending on configuration.
#' @param name A character vector of names or aliases to test.
#' @return A character vector of formatted names.
formatName=function(name) {

    name <- tolower(name)
    name <- gsub('[^a-z0-9]', private$field.name.sep, name)

    return(name)
},

#' @description
#' Tests if names are defined fields.
#' @param name A character vector of names or aliases to test.
#' @return A logical vector, the same length as `name`, with TRUE
#'     for name values that corresponds to a defined field.
isDefined=function(name) {

    name <- self$formatName(name)

    return(name %in% names(private$fields) | self$isAlias(name))
},

#' @description
#' Tests if names are valid defined fields. Throws an error if any name
#'     does not correspond to a defined field.
#' @param name A character vector of names or aliases to test.
#' @return Nothing.
checkIsDefined=function(name) {

    def <- self$isDefined(name)
    if (any( ! def))
        error0("Field(s) \"", paste(name[ ! def], collapse=", "),
            "\" is/are not defined.")

    return(invisible(NULL))
},

#' @description
#' Gets the real names (main names) of fields. If some name is not found
#' neither in aliases nor in real names, an error is thrown.
#' @param name A character vector of names or aliases.
#' @param fail Fails if name is unknown.
#' @return A character vector, the same length as `name`, with the real
#' field name for each name given (i.e. each alias is replaced with the
#' real name).
getRealName=function(name, fail=TRUE) {
    chk::chk_character(name)
    chk::chk_flag(fail)

    name <- self$formatName(name)

    # Check name
    if (fail)
        self$checkIsDefined(name)

    # Get real name
    isNotRealName <-  ! name %in% names(private$fields) 
    if (any(isNotRealName)) {
        realName <- private$aliasToName[name[isNotRealName]]
        if (any(is.na(realName)) && fail)
            error("Unknown fields: %s.",
                paste(name[isNotRealName][is.na(realName)], collapse=', '))
        name[isNotRealName][ ! is.na(realName)] <- realName[ ! is.na(realName)]
    }

    return(name)
},

#' @description
#' Gets a BiodbEntryField instance.
#' @param name A character vector of names or aliases.
#' @param drop If TRUE and only one name has been submitted, returns a
#' single BiodbEntryField instance instead of a list.
#' @return A named list of BiodbEntryField instances. The names of the
#' list are the real names of the entry fields, thus they may be
#' different from the one provided inside the name argument.
get=function(name, drop=TRUE) {
    chk::chk_character(name)
    chk::chk_flag(drop)

    biodb::logTrace('Asked field names are: %s.', paste(name, collapse=', '))
    name <- self$getRealName(name)
    biodb::logTrace('Realnames of fields are: %s.', paste(name, collapse=', '))
    fields <- private$fields[tolower(name)]
    biodb::logTrace('%d fields were returned.', length(fields))
    biodb::logTrace('fields variable is a %s.', class(fields))
    if (drop && length(fields) == 1)
        fields <- fields[[1]]

    return(fields)
},

#' @description
#' Gets the main names of all fields.
#' @param type Set this parameter to a character vector in order to
#' return only the names of the fields corresponding to the types
#' specified.
#' @param computable If set to TRUE, returns only the names of computable
#' fields. If set to FALSE, returns only the names of fields that are not
#' computable.
#' @return A character vector containing all selected field names.
getFieldNames=function(type=NULL, computable=NULL) {

    # Filter by type
    if ( ! is.null(type)) {
        fields <- character()
        for (n in names(private$fields))
            if (private$fields[[n]]$getType() %in% type)
                fields <- c(fields, n)
    }

    else
        fields <- names(private$fields)

    # Filter on computability
    if ( ! is.null(computable)) {
        fct <- function(f) {
            private$fields[[f]]$isComputable()
        }
        fields <- Filter(fct, fields)
    }

    return(sort(fields))
},

#' @description
#' Gets a database ID field.
#' @param database The name (i.e.: Biodb ID) of a database.
#' @return 
#'     accession numbers) for this database.
getDatabaseIdField=function(database) {

    dbs <- private$parent$getDbsInfo()
    return(self$get(dbs$get(database)$getIdFieldName()))
},

#' @description
#' Prints information about the instance.
#' @return Nothing.
print=function() {

    cat("Biodb entry fields information instance.\n")

    return(invisible(NULL))
},

#' @description
#' Defines fields.
#' @param def A named list of field definitions. The names of the list are the
#'     main names of the fields.
#' @return Nothing.
define=function(def) {

    # Loop on all fields
    for (f in names(def)) {
        logDebug("Define field %s.", f)
        args <- def[[f]]
        args[['name']] <- f
        do.call(private$defineField, args)
    }

    return(invisible(NULL))
},

#' @description
#' Terminates the instance. This method will be called
#'     automatically by the BiodbMain instance when you call
#' @param BiodbMain :terminate().
#' @return Nothing.
terminate=function() {

    return(invisible(NULL))
}
),

private=list(
    fields=NULL,
    aliasToName=NULL,
    field.name.sep=NULL,
    parent=NULL
,
defineField=function(name, ...) {

    name <- self$formatName(name)

    # Create new field instance
    field <- BiodbEntryField$new(parent=self, name=name, ...)

    # Is field already defined?
    if (self$isDefined(name)) {
        private$fields[[name]]$equals(field, fail=TRUE)
        private$fields[[name]]$updateWithValuesFrom(field)
    }

    # Register new field
    else {
        # Register new field inside fields list
        private$fields[[name]] <- field

        # Define aliases
        if (field$hasAliases())
            for (alias in field$getAliases())
                private$aliasToName[[alias]] <- name
    }
    
    # Check
    private$fields[[name]]$check()
}
))
