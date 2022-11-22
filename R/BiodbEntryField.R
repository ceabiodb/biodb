#' A class for describing an entry field.
#'
#' This class is used by \code{\link{BiodbEntryFields}} for storing field
#' characteristics, and returning them through the \code{get()} method. The
#' constructor is not meant to be used, but for development purposes the
#' constructor's parameters are nevertheless described in the Fields section.
#'
#' The constructor accepts the following arguments:
#'
#' name: The name of the field.
#'
#' alias: A character vector containing zero or more aliases for the field.
#'
#' type: A type describing the field. One of: "mass", "name" or "id". Optional.
#'
#' class: The class of the field. One of: "character", "integer", "double",
#' "logical", "object", "data.frame".
#'
#' card: The cardinality of the field: either "1" or "*".
#'
#' forbids.duplicates: If set to TRUE, the field forbids duplicated values.
#'
#' description: A description of the field.
#'
#' allowed.values: The values authorized for the field.
#'
#' lower.case: Set to TRUE if you want all values set to the field to be forced
#' to lower case.
#'
#' case.insensitive: Set to TRUE of you want the field to ignore case when
#' checking a value.
#'
#' computable.from: The Biodb ID of a database, from which this field can be
#' computed.
#'
#' virtual: If set to \code{TRUE}, the field is computed from other fields, and
#' thus cannot be modified.
#'
#' virtual.group.by.type: For a virtual field of class data.frame, this
#' indicates to gather all fields of the specified type to build a data frame.
#'
#' @seealso Parent class \code{\link{BiodbEntryFields}}.
#'
#' @examples
#' # Get the class of the InChI field.
#' mybiodb <- biodb::newInst()
#' inchi.field.class <- mybiodb$getEntryFields()$get('inchi')$getClass()
#'
#' # Test the cardinality of a field
#' card.one <- mybiodb$getEntryFields()$get('name')$hasCardOne()
#' card.many <- mybiodb$getEntryFields()$get('name')$hasCardMany()
#'
#' # Get the description of a field
#' desc <- mybiodb$getEntryFields()$get('inchi')$getDescription()
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import R6
#' @export
BiodbEntryField <- R6::R6Class("BiodbEntryField",

public=list(

#' @description
#' New instance initializer. This class must not be instantiated directly.
#' Instead, you access the instances of this class through the
#' BiodbEntryFields instance that you get from the BiodbMain instance.
#' @param parent The BiodbEntryFields parent instance.
#' @param name The field name.
#' @param alias The field aliases as a character vector.
#' @param type The field type.
#' @param class The field class.
#' @param card The field cardinality.
#' @param forbids.duplicates Set to TRUE to forbid duplicated values.
#' @param description The field description.
#' @param allowed.values Restrict possible values to a set of allowed
#' values.
#' @param lower.case All values will be converted to lower case.
#' @param case.insensitive Comparison will be made case insensitive for
#' this field.
#' @param computable.from A list of databases from which to compute
#' automatically the value of this field.
#' @param virtual Set to TRUE if this field is virtual.
#' @param virtual.group.by.type In case of a virtual field, set the type
#' of fields to group together into a data frame.
#' @param dataFrameGroup The data frame group.
#' @return Nothing.
initialize=function(parent, name, alias=NA_character_, type=NA_character_,
    class=c('character', 'integer', 'double', 'logical', 'object',
        'data.frame'),
    card=c('one', 'many'), forbids.duplicates=FALSE, description=NA_character_,
    allowed.values=NULL, lower.case=FALSE, case.insensitive=FALSE,
    computable.from=NULL, virtual=FALSE, virtual.group.by.type=NULL,
    dataFrameGroup=NA_character_) {

    private$parent <- parent

    # Set name
    if ( is.null(name) || is.na(name) || nchar(name) == '')
        error0("You cannot set an empty name for a field. Name was',
            ' empty (either NULL or NA or empty string).")
    private$name <- tolower(name)

    # Set type
    private$type <- type

    # Set class
    class <- match.arg(class)
    private$class <- class

    # Set cardinality
    card <- match.arg(card)
    if (private$class == 'data.frame' && card != 'one')
        error0('Cardinality "', card, '" is forbidden for class "',
                    private$class, '" for field "', name, '"')
    private$cardinality <- card

    # Set description
    private$description <- description

    # Set alias
    if (length(alias) > 1 && any(is.na(alias)))
        error0("One of the aliases of entry field \"", name, "\" is NA.")
    private$alias <- alias

    # Set allowed values
    if ( ! is.null(allowed.values)) {
        if ( ! is.vector(allowed.values, mode='numeric')
            && ! is.vector(allowed.values, mode='character')
            && ! is.vector(allowed.values, mode='list'))
            error0('Allowed values must be either a list, a numeric',
                ' vector or a character vector.')

        # For a list check that all values are character vectors
        if (is.vector(allowed.values, mode='list')) {
            if (is.null(names(allowed.values)))
                error0('When allowed values are specified as a list,',
                    ' names must be set.')
            if ( ! all(vapply(allowed.values,
                function(x) is.vector(x, 'character'), FUN.VALUE=TRUE)))
                error0('When allowed values are specified as a list,',
                    ' all values must be characters.')
        }
    }
    private$allowed.values <- allowed.values

    # Case insensitive
    if (case.insensitive && class != 'character')
        error0('Only character fields can be case insensitive.')
    private$case.insensitive <- case.insensitive

    # Lower case
    if (lower.case && class != 'character')
        error0('Only character fields can be forced to lower case.')
    private$lower.case <- lower.case

    # Computable from
    private$setComputableFrom(computable.from)

    # Virtual
    private$virtual <- virtual
    private$virtualGroupByType <- if (is.null(virtual.group.by.type))
        character() else virtual.group.by.type
    if ( ! private$virtual && length(private$virtualGroupByType) > 0)
        error0('virtual.group.by.type is not usable with non-virtual field "',
            name, '".')
    if (length(private$virtualGroupByType) > 0 && private$class != 'data.frame')
        error0('virtual.group.by.type is only usable for virtual field of',
            ' class data.frame. Error for field "', name, '".')

    # Set other fields
    private$forbids.duplicates <- forbids.duplicates
    private$dataFrameGroup <- dataFrameGroup

    return(invisible(NULL))
},

#' @description
#' Gets the name.
#' @return The name of this field.
getName=function() {

    return(private$name)
},

#' @description
#' Gets field's type.
#' @return The type of this field.
getType=function() {

    return(private$type)
}

#' @description
#' Tests if this field is of the specified type.
#' @param type The type.
#' @return TRUE if this field is of the specified type, FALSE otherwise.
,isOfType=function(type) {

    chk::chk_string(type)
    
    return( ! is.null(private$type) && ! is.na(private$type)
        && private$type == type)
}


#' @description
#' Get field's description.
#' @return The description of this field.
,getDescription=function() {

    return(private$description)
},

#' @description
#' Tests if this field has aliases.
#' @return TRUE if this entry field defines aliases, FALSE otherwise.
hasAliases=function() {

    return( ! any(is.na(private$alias)))
},

#' @description
#' Get aliases.
#' @return The list of aliases if some are defined, otherwise returns
#'     NULL."
getAliases=function() {
    aliases <- NULL

    if (self$hasAliases())
        aliases <- private$alias

    return(aliases)
},

#' @description
#' Adds an alias to the list of aliases. 
#' @param alias The name of a valid alias.
#' @return Nothing.
addAlias=function(alias) {

    if ( ! alias %in% private$alias) {
        
        # Check that alias does not already exist
        if (private$parent$isAlias(alias))
            error0("Alias ", alias, " already exists.")

        # Add alias
        if ( ! alias %in% private$alias)
            private$alias <- c(private$alias, alias)
    }

    return(invisible(NULL))
},

#' @description
#' Removes an alias from the list of aliases. 
#' @param alias The name of a valid alias.
#' @return Nothing.
removeAlias=function(alias) {
    
    if (alias %in% private$alias)
        private$alias <- private$alias[private$alias != alias]

    return(invisible(NULL))
},

#' @description
#' Gets all names.
#' @return The list of all names (main name and aliases).
getAllNames=function() {

    aliases <- self$getAliases()
    names <- self$getName()
    if ( ! is.null(aliases))
        names <- c(names, aliases)

    return(names)
},

#' @description
#' Tests if this field is computable from another field or another
#'     database.
#' @return TRUE if the field is computable, FALSE otherwise.
isComputable=function() {

    return( ! is.null(private$computable.from))
},

#' @description
#' Get the list of connectors that can be used to compute this field.
#' @return A list of list objects. Each list object contains the name of the
#' database from which the field is computable.
getComputableFrom=function() {
    return(private$computable.from)
},

#' @description
#' Gets the defined data frame group, if any.
#' @return The data frame group, as a character value.
getDataFrameGroup=function() {

    return(private$dataFrameGroup)
},

#' @description
#' Gets the ID of the database from which this field can be computed.
#' @return The list of databases where to find this field's value.
isComputableFrom=function() {

    return(private$computable.from)
},

#' @description
#' Adds a directive from the list of computableFrom.
#' @param directive A valid \"computable from\" directive.
#' @return Nothing.
addComputableFrom=function(directive) {

    # Has a "database" field
    if ( ! 'database' %in% names(directive))
        error0('You must specified the database for directive',
            ', for field "', private$name, '".')

    # Search if the directive exists
    for (d in private$computable.from) {
        if (d$database == directive$database)
            error(paste0('A "computable from" directive already',
                'exists for database "%s".'), d$database)
    }

    # Add the new directive
    private$computable.from <- if (is.null(private$computable.from))
        list(directive) else c(private$computable.from, directive)

    return(invisible(NULL))
},

#' @description
#' Removes a directive from the list of computableFrom.
#' @param directive A valid \"computable from\" directive.
#' @return Nothing.
removeComputableFrom=function(directive) {

    # Search for directive
    n <- 0
    for (d in private$computable.from) {
        n <- n + 1
        if (d$database == directive$database) {
            private$computable.from[n] <- NULL
            break
        }
    }

    return(invisible(NULL))
},

#' @description
#' Corrects a value so it is compatible with this field.
#' @param value A value.
#' @return The corrected value.
correctValue=function(value) {

    if (self$isAtomic() && ! is.null(value)
        && ! (length(value) == 1 && is.na(value))) {

        # Correct type
        if (self$getClass() != class(value))
            value <- as.vector(value, mode=self$getClass())

        # Lower case
        if (private$lower.case)
            value <- tolower(value)

        # Enumerated type
        if (self$isEnumerate() && methods::is(private$allowed.values, 'list')) {
            fct <- function(v) {
                for (a in names(private$allowed.values))
                    if (v == a || v %in% private$allowed.values[[a]])
                        return(a)
                return(v)
            }
            fv <- as.vector(0, mode=self$getClass())
            value <- vapply(value, fct, FUN.VALUE=fv, USE.NAMES=FALSE)
        }
    }

    return(value)
},

#' @description
#' Tests if this field is an enumerate type (i.e.: it defines allowed
#'     values).
#' @return TRUE if this field defines some allowed values, FALSE
#'     otherwise.
isEnumerate=function() {

    return( ! is.null(private$allowed.values))
},

#' @description
#' Tests if this field is a virtual field.
#' @return TRUE if this field is virtual, FALSE
#'     otherwise.
isVirtual=function() {

    return(private$virtual)
},

#' @description
#' Gets type for grouping field values when building a virtual data
#'     frame.
#' @return The type, as a character value.
getVirtualGroupByType=function() {

    return(private$virtualGroupByType)
},

#' @description
#' Gets allowed values.
#' @param value If this parameter is set to particular allowed values, then the
#'     method returns a list of synonyms for this value (if any).
#' @return A character vector containing all allowed values.
getAllowedValues=function(value=NULL) {

    values <- NULL
    if ( ! is.null(private$allowed.values)) {

        # Take all values
        if (is.null(value)) {
            values <- unlist(private$allowed.values)
            if ( ! is.null(names(private$allowed.values)))
                values <- c(values, names(private$allowed.values))
            names(values) <- NULL

        # Get all allowed values for just one specific value
        #    (i.e.: get synonyms)
        } else {

            # Find value in keys
            if ( ! is.null(names(private$allowed.values))
                && value %in% names(private$allowed.values))
                values <- c(value, unlist(private$allowed.values[[value]]))

            # Search value in values
            else {
                for (i in seq_along(private$allowed.values))
                    if (value %in% private$allowed.values[[i]]) {
                        values <- unlist(private$allowed.values[[i]])
                        if ( ! is.null(names(private$allowed.values)))
                            values <- c(names(private$allowed.values)[[i]],
                                        values)
                        break
                    }
            }
        }
    }

    return(values)
},

#' @description
#' Adds an allowed value, as a synonym to already an existing value. Note
#' that not all enumerate fields accept synonyms.
#' @param key The key associated with the value (i.e.: the key is the
#' main name of an allowed value).
#' @param value The new value to add.
#' @return Nothing.
addAllowedValue=function(key, value) {

    key <- tolower(key)
    if (private$lower.case)
        value <- tolower(value)

    # Check that key exists
    if (is.null(names(private$allowed.values)))
        error0('Field "', private$name,
            '" doesn\'t use keys for its allowed values.')
    if ( ! key %in% names(private$allowed.values))
        error0('Field "', private$name, '" doesn\'t use key "', key,
            '" for its allowed values.')

    # Check that value is not already used
    if (value %in% self$getAllowedValues()) {
        current.key <- self$correctValue(value)
        if (current.key != key)
            error0('Field "', private$name, '" already uses value "', value,
                '" for its allowed values, but with key "', current.key,
                '" instead of key "', key, '".')
        else
            logInfo0('Field "', private$name, '" already uses value "', value,
                '" for its allowed values, with key "', key, '".')
    }

    # Add new value
    private$allowed.values[[key]] <- c(private$allowed.values[[key]], value)

    return(invisible(NULL))
},

#' @description
#' Checks if a value is correct. Fails if `value` is incorrect.
#' @param value The value to check.
#' @return Nothing.
checkValue=function(value) {

    if (private$lower.case)
        value <- tolower(value)

    bad.values <- value[ ! value %in% self$getAllowedValues()]
    if (self$isEnumerate() && length(bad.values) > 0) {
        bv <- paste(bad.values[ ! duplicated(bad.values)], collapse=', ')
        av <- paste(self$getAllowedValues(), collapse=', ')
        error0('Value(s) ', bv, ' is/are not allowed for field ',
            self$getName(), '. Allowed values are: ', av, '.')
    }

    return(invisible(NULL))
},

#' @description
#' Tests if this field has a cardinality of one.
#' @return TRUE if the cardinality of this field is one, FALSE
#'     otherwise.
hasCardOne=function() {

    return(private$cardinality == 'one')
},

#' @description
#' Tests if this field has a cardinality greater than one.
#' @return TRUE if the cardinality of this field is many, FALSE
#'     otherwise.
hasCardMany=function() {

    return(private$cardinality == 'many')
},

#' @description
#' Tests if this field forbids duplicates.
#' @return TRUE if this field forbids duplicated values, FALSE
#'     otherwise.
forbidsDuplicates=function() {

    return(private$forbids.duplicates)
},

#' @description
#' Tests if this field is case sensitive.
#' @return TRUE if this field is case insensitive, FALSE otherwise.
isCaseInsensitive=function() {

    return(private$case.insensitive)
},

#' @description
#' Gets the class of this field's value.
#' @return class) of this field.
getClass=function() {

    return(private$class)
},

#' @description
#' Tests if this field's type is a class.
#' @return TRUE if field's type is a class, FALSE otherwise.
isObject=function() {

    return(private$class == 'object')
},

#' @description
#' Tests if this field's type is `data.frame`.
#' @return TRUE if field's type is data frame, FALSE otherwise."
isDataFrame=function() {
    return(private$class == 'data.frame')
},

#' @description
#' Tests if this field's type is an atomic  type.
#' @return character,
#'     integer, double or logical), FALSE otherwise.
isAtomic=function() {

    return(private$class %in% c('character', 'integer', 'double', 'logical'))
},

#' @description
#' Tests if this field's type is a basic vector type.
#' @return character,
#'     integer, double or logical), FALSE otherwise.
isVector=function() {

    lifecycle::deprecate_soft('1.0.0', 'isVector()', 'isAtomic()')

    return(self$isAtomic())
},

#' @description
#' Compares this instance with another, and tests if they are equal.
#' @param other Another BiodbEntryField instance.
#' @param fail If set to TRUE, then throws error instead of returning FALSE.
#' @return TRUE if they are equal, FALSE otherwise.
equals=function(other, fail=FALSE) {

    if ( ! methods::is(other, "BiodbEntryField"))
        error("Parameter `other` must be an instance of BiodbEntryField.")

    eq <- TRUE

    # Fields to test
    fields <- c('name', 'type', 'class', 'cardinality', 'forbids.duplicates',
                'allowed.values', 'lower.case', 'case.insensitive')

    # Loop on all fields
    for (f in fields) {
        a <- self[[f]]
        b <- other[[f]]
        if ( ! ((is.na(a) && is.na(b)) || (is.null(a) && is.null(b))
            || ( ! is.na(a) && ! is.na(b) && ! is.null(a) && ! is.null(b)
            && a == b))) {
            eq <- FALSE
            break
        }
    }

    if (fail && ! eq)
        error0("Field \"", other[['name']], "\" has already been defined.")

    return(eq)
},

#' @description
#' Updates fields using values from `other` instance. The updated fields
#' @param are 'alias' and 'computable.from'. No values will be removed
#' from those vectors. The new values will only be appended. This allows
#' to extend an existing field inside a new connector definition.
#' @param other Another BiodbEntryField instance.
#' @return Nothing.
updateWithValuesFrom=function(other) {

    if ( ! methods::is(other, "BiodbEntryField"))
        error("Parameter `other` must be an instance of BiodbEntryField.")

    # Update fields
    for (a in other$.alias)
        self$addAlias(a)
    if ( ! is.null(other$getComputableFrom()))
        for (directive in other$getComputableFrom())
            self$addComputableFrom(directive)

    return(invisible(NULL))
},

#' @description
#' Print informations about this entry.
#' @return Nothing.
print=function() {

    cat("Entry field \"", private$name, "\".\n", sep='')
    cat("  Description: ", private$description, "\n", sep='')
    cat("  Class: ", private$class, ".\n", sep='')
    if (private$virtual) {
        cat("Virtual.")
        if ( ! is.null(private$virtualGroupByType))
            cat('Grouped by type "', private$virtualGroupByType, '".')
        cat("\n")
    }
    if (private$class == 'character') {
        case <-  if (private$case.insensitive) 'insensitive' else 'sensitive'
        lower <- if (private$lower.case) ' Value will be forced to lower case.'
            else ''
        cat("  Case: ", case, '.', lower, "\n", sep='')
    }
    if ( ! is.na(private$type))
        cat("  Type: ", private$type, ".\n", sep='')
    cat("  Cardinality: ", private$cardinality, ".\n", sep='')
    if (private$cardinality == 'many')
        cat("  Duplicates: ", if (private$forbids.duplicates) 'forbidden' else
            'allowed', ".\n", sep='')
    cat("  Aliases: ", paste(private$alias, collapse=', '), ".\n", sep='')
    if ( ! is.null(private$allowed.values))
        cat("  Allowed values: ", paste(private$allowed.values, collapse=', '),
            ".\n", sep='')

    return(invisible(NULL))
},

#' @description
#' Gets the field's cardinality.
#' @return The cardinality: "one" or "many".
getCardinality=function() {
    return(private$cardinality)
},

#' @description
#' Checks if essential values are defined.
#' @return Nothing.
check=function() {
    
    # Check name
    if (is.null(private$name) || is.na(private$name) || private$name == '')
        warn("Missing name for entry field.")
    
    # Check description
    if (is.null(private$description) || is.na(private$description)
        || private$description == '')
        warn('Missing description for entry field "%s".', private$name)

    return(invisible(NULL))
}
),

private=list(
    name=NULL,
    type=NULL,
    class=NULL,
    cardinality=NULL,
    description=NULL,
    alias=NULL,
    parent=NULL,
    dataFrameGroup=NULL,
    virtual=NULL,
    virtualGroupByType=NULL,
    allowed.values=NULL,
    case.insensitive=NULL,
    lower.case=NULL,
    computable.from=NULL,
    forbids.duplicates=NULL,

setComputableFrom=function(computable.from) {

    if ( ! is.null(computable.from)) {

        # Is a list
        if ( ! is.list(computable.from) || ! is.null(names(computable.from)))
            error0('computable.from must be an unnamed list, for field "',
                private$name, '".')

        # Loop on all directives
        for (directive in computable.from) {

            # Has a "database" field
            if ( ! 'database' %in% names(directive))
                error0('You must specified the database for directive',
                    ', for field "', private$name, '".')

            # Check list of fields
            if ('fields' %in% names(directive)
                && ! is.character(directive$fields))
                error0('In directive of field "', private$name,
                    '", "fields" must be a list of field names.')
        }
    }

    private$computable.from <- computable.from

    return(invisible(NULL))
}
))
