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
#' virtual.group.by.type: For a virtual field of class data.frame, this indicates to
#' gather all fields of the specified type to build a data frame.
#'
#' @seealso Parent class \code{\link{BiodbEntryFields}}.
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
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @include BiodbChildObject.R
#' @export BiodbEntryField
#' @exportClass BiodbEntryField
BiodbEntryField <- methods::setRefClass("BiodbEntryField",
    contains="BiodbChildObject",
    fields=list(
        .name='character',
        .type='character',
        .class='character',
        .cardinality='character',
        .forbids.duplicates='logical',
        .description='character',
        .alias='character',
        .allowed.values="ANY",
        .lower.case='logical',
        .case.insensitive='logical',
        .computable.from='ANY',
        dataFrameGroup='character',
        virtual='logical',
        virtualGroupByType='character'
        ),

methods=list(

initialize=function(name, alias=NA_character_, type=NA_character_,
                    class=c('character', 'integer', 'double', 'logical',
                            'object', 'data.frame'), card=c('one', 'many'),
                    forbids.duplicates=FALSE, description=NA_character_,
                    allowed.values=NULL, lower.case=FALSE,
                    case.insensitive=FALSE, computable.from=NULL, virtual=FALSE,
                    virtual.group.by.type=NULL, dataFrameGroup=NA_character_,...) {

    callSuper(...)

    # Set name
    if ( is.null(name) || is.na(name) || nchar(name) == '')
        error0("You cannot set an empty name for a field. Name was',
              ' empty (either NULL or NA or empty string).")
    .self$.name <- tolower(name)

    # Set type
    .self$.type <- type

    # Set class
    class <- match.arg(class)
    .self$.class <- class

    # Set cardinality
    card <- match.arg(card)
    if (.self$.class == 'data.frame' && card != 'one')
        error0('Cardinality "', card, '" is forbidden for class "',
                    .self$.class, '" for field "', name, '"')
    .self$.cardinality <- card

    # Set description
    .self$.description <- description

    # Set alias
    if (length(alias) > 1 && any(is.na(alias)))
        error0("One of the aliases of entry field \"", name, "\" is NA.")
    .self$.alias <- alias

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
                              function(x) is.vector(x, 'character'),
                              FUN.VALUE=TRUE)))
                error0('When allowed values are specified as a list,',
                      ' all values must be characters.')
        }
    }
    .self$.allowed.values <- allowed.values

    # Case insensitive
    if (case.insensitive && class != 'character')
        error0('Only character fields can be case insensitive.')
    .self$.case.insensitive <- case.insensitive

    # Lower case
    if (lower.case && class != 'character')
        error0('Only character fields can be forced to lower case.')
    .self$.lower.case <- lower.case

    # Computable from
    .self$.setComputableFrom(computable.from)

    # Virtual
    .self$virtual <- virtual
    .self$virtualGroupByType <- if (is.null(virtual.group.by.type)) character() else virtual.group.by.type
    if ( ! .self$virtual && length(.self$virtualGroupByType) > 0)
        error0('virtual.group.by.type is not usable with non-virtual field "',
              name, '".')
    if (length(.self$virtualGroupByType) > 0 && .self$.class != 'data.frame')
        error0('virtual.group.by.type is only usable for virtual field of class',
              ' data.frame. Error for field "', name, '".')

    # Set other fields
    .self$.forbids.duplicates <- forbids.duplicates
    .self$dataFrameGroup <- dataFrameGroup
},

.setComputableFrom=function(computable.from) {

    if ( ! is.null(computable.from)) {

        # Is a list
        if ( ! is.list(computable.from) || ! is.null(names(computable.from)))
            error0('computable.from must be an unnamed list, for field "',
                  .self$.name, '".')

        # Loop on all directives
        for (directive in computable.from) {

            # Has a "database" field
            if ( ! 'database' %in% names(directive))
                error0('You must specified the database for directive',
                       ', for field "', .self$.name, '".')

            # Check list of fields
            if ('fields' %in% names(directive)
                && ! is.character(directive$fields))
                error0('In directive of field "', .self$.name,
                      '", "fields" must be a list of field names.')
        }
    }

    .self$.computable.from <- computable.from
},

getName=function() {
    ":\n\nGets the name.
    \nReturned value: The name of this field.
    "

    return(.self$.name)
},

getType=function() {
    ":\n\nGets field's type.
    \nReturned value: The type of this field.
    "

    return(.self$.type)
},

getDescription=function() {
    ":\n\nGet field's description.
    \nReturned value: The description of this field.
    "

    return(.self$.description)
},

hasAliases=function() {
    ":\n\nTests if this field has aliases.
    \nReturned value: TRUE if this entry field defines aliases, FALSE otherwise.
    "

    return( ! any(is.na(.self$.alias)))
},

getAliases=function() {
    ":\n\nGet aliases.
    \nReturned value: The list of aliases if some are defined, otherwise returns
    NULL."

    aliases <- NULL

    if (.self$hasAliases())
        aliases <- .self$.alias

    return(aliases)
},

addAlias=function(alias) {
    ":\n\nAdds an alias to the list of aliases. 
    \nalias: The name of a valid alias.
    \nReturned value: None.
    "

    if ( ! alias %in% .self$.alias) {
        
        # Check that alias does not already exist
        if (.self$getParent()$isAlias(alias))
            error0("Alias ", alias, " already exists.")

        # Add alias
        if ( ! alias %in% .self$.alias)
            .self$.alias <- c(.self$.alias, alias)
    }
},

removeAlias=function(alias) {
    ":\n\nRemoves an alias from the list of aliases. 
    \nalias: The name of a valid alias.
    \nReturned value: None.
    "
    
    if (alias %in% .self$.alias)
        .self$.alias <- .self$.alias[.self$.alias != alias]
},

getAllNames=function() {
    ":\n\nGets all names.
    \nReturned value: The list of all names (main name and aliases).
    "

    aliases <- .self$getAliases()
    names <- .self$getName()
    if ( ! is.null(aliases))
        names <- c(names, aliases)

    return(names)
},

isComputable=function() {
    ":\n\nTests if this field is computable from another field or another
    database.
    \nReturned value: TRUE if the field is computable, FALSE otherwise.
    "

    return( ! is.null(.self$.computable.from))
},

getDataFrameGroup=function() {
    ":\n\nGets the defined data frame group, if any.
    \nReturned value: The data frame group, as a character value.
    "

    return(.self$dataFrameGroup)
},

isComputableFrom=function() {
    ":\n\nGets the ID of the database from which this field can be computed.
    \nReturned value: The list of databases where to find this field's value.
    "

    return(.self$.computable.from)
},

addComputableFrom=function(directive) {
    ":\n\nAdds a directive from the list of computableFrom.
    \ndirective: A valid \"computable from\" directive.
    \nReturned value: None.
    "

    # Has a "database" field
    if ( ! 'database' %in% names(directive))
        error0('You must specified the database for directive',
              ', for field "', .self$.name, '".')

    # Search if the directive exists
    for (d in .self$.computable.from) {
        if (d$database == directive$database)
            error(paste0('A "computable from" directive already',
                         'exists for database "%s".'), d$database)
    }

    # Add the new directive
    .self$.computable.from <- if (is.null(.self$.computable.from))
        list(directive) else c(.self$.computable.from, directive)
},

removeComputableFrom=function(directive) {
    ":\n\nRemoves a directive from the list of computableFrom.
    \ndirective: A valid \"computable from\" directive.
    \nReturned value: None.
    "

    # Search for directive
    n <- 0
    for (d in .self$.computable.from) {
        n <- n + 1
        if (d$database == directive$database) {
            .self$.computable.from[n] <- NULL
            break
        }
    }
},

correctValue=function(value) {
    ":\n\nCorrects a value so it is compatible with this field.
    \nvalue: A value.
    \nReturned value: The corrected value.
    "

    if (.self$isVector() && ! is.null(value) && ! (length(value) == 1
                                                   && is.na(value))) {

        # Correct type
        if (.self$getClass() != class(value))
            value <- as.vector(value, mode=.self$getClass())

        # Lower case
        if (.self$.lower.case)
            value <- tolower(value)

        # Enumerated type
        if (.self$isEnumerate() && methods::is(.self$.allowed.values, 'list')) {
            fct <- function(v) {
                for (a in names(.self$.allowed.values))
                    if (v == a || v %in% .self$.allowed.values[[a]])
                        return(a)
                return(v)
            }
            fv <- as.vector(0, mode=.self$getClass())
            value <- vapply(value, fct, FUN.VALUE=fv, USE.NAMES=FALSE)
        }
    }

    return(value)
},

isEnumerate=function() {
    ":\n\nTests if this field is an enumerate type (i.e.: it defines allowed
    values).
    \nReturned value: TRUE if this field defines some allowed values, FALSE
    otherwise.
    "

    return( ! is.null(.self$.allowed.values))
},

isVirtual=function() {
    ":\n\nTests if this field is a virtual field.
    \nReturned value: TRUE if this field is virtual, FALSE
    otherwise.
    "

    return(.self$virtual)
},

getVirtualGroupByType=function() {
    ":\n\nGets type for grouping field values when building a virtual data frame.
    \nReturned value: The type, as a character value.
    "

    return(.self$virtualGroupByType)
},

getAllowedValues=function(value=NULL) {
    ":\n\nGets allowed values.
    \nvalue: If this parameter is set to particular allowed values, then the
    method returns a list of synonyms for this value (if any).
    \nReturned value: A character vector containing all allowed values.
    "

    values <- NULL
    if ( ! is.null(.self$.allowed.values)) {

        # Take all values
        if (is.null(value)) {
            values <- unlist(.self$.allowed.values)
            if ( ! is.null(names(.self$.allowed.values)))
                values <- c(values, names(.self$.allowed.values))
            names(values) <- NULL

        # Get all allowed values for just one specific value
        #    (i.e.: get synonyms)
        } else {

            # Find value in keys
            if ( ! is.null(names(.self$.allowed.values))
                && value %in% names(.self$.allowed.values))
                values <- c(value, unlist(.self$.allowed.values[[value]]))

            # Search value in values
            else {
                for (i in seq_along(.self$.allowed.values))
                    if (value %in% .self$.allowed.values[[i]]) {
                        values <- unlist(.self$.allowed.values[[i]])
                        if ( ! is.null(names(.self$.allowed.values)))
                            values <- c(names(.self$.allowed.values)[[i]],
                                        values)
                        break
                    }
            }
        }
    }

    return(values)
},

addAllowedValue=function(key, value) {
    ":\n\nAdds an allowed value, as a synonym to already an existing value. Note
    that not all enumerate fields accept synonyms.
    \nkey: The key associated with the value (i.e.: the key is the main name of
    an allowed value).
    \nvalue: The new value to add.
    \nReturned value: None.
    "

    key <- tolower(key)
    if (.self$.lower.case)
        value <- tolower(value)

    # Check that key exists
    if (is.null(names(.self$.allowed.values)))
        error0('Field "', .self$.name,
              '" doesn\'t use keys for its allowed values.')
    if ( ! key %in% names(.self$.allowed.values))
        error0('Field "', .self$.name, '" doesn\'t use key "', key,
              '" for its allowed values.')

    # Check that value is not already used
    if (value %in% .self$getAllowedValues()) {
        current.key <- .self$correctValue(value)
        if (current.key != key)
            error0('Field "', .self$.name, '" already uses value "', value,
                  '" for its allowed values, but with key "', current.key,
                  '" instead of key "', key, '".')
        else
            logInfo0('Field "', .self$.name, '" already uses value "', value,
                    '" for its allowed values, with key "', key, '".')
    }

    # Add new value
    .self$.allowed.values[[key]] <- c(.self$.allowed.values[[key]], value)
},

checkValue=function(value) {
    ":\n\nChecks if a value is correct. Fails if `value` is incorrect.
    \nvalue: The value to check.
    \nReturned value: None.
    "

    if (.self$.lower.case)
        value <- tolower(value)

    bad.values <- value[ ! value %in% .self$getAllowedValues()]
    if (.self$isEnumerate() && length(bad.values) > 0) {
        bv <- paste(bad.values[ ! duplicated(bad.values)], collapse=', ')
        av <- paste(.self$getAllowedValues(), collapse=', ')
        error0('Value(s) ', bv, ' is/are not allowed for field ',
              .self$getName(), '. Allowed values are: ', av, '.')
    }
},

hasCardOne=function() {
    ":\n\nTests if this field has a cardinality of one.
    \nReturned value: TRUE if the cardinality of this field is one, FALSE
    otherwise.
    "

    return(.self$.cardinality == 'one')
},

hasCardMany=function() {
    ":\n\nTests if this field has a cardinality greater than one.
    \nReturned value: TRUE if the cardinality of this field is many, FALSE
    otherwise.
    "

    return(.self$.cardinality == 'many')
},

forbidsDuplicates=function() {
    ":\n\nTests if this field forbids duplicates.
    \nReturned value: TRUE if this field forbids duplicated values, FALSE
    otherwise.
    "

    return(.self$.forbids.duplicates)
},

isCaseInsensitive=function() {
    ":\n\nTests if this field is case sensitive.
    \nReturned value: TRUE if this field is case insensitive, FALSE otherwise.
    "

    return(.self$.case.insensitive)
},

getClass=function() {
    ":\n\nGets the class of this field's value.
    \nReturned value: The type (i.e.: class) of this field.
    "

    return(.self$.class)
},

isObject=function() {
    ":\n\nTests if this field's type is a class.
    \nReturned value: TRUE if field's type is a class, FALSE otherwise.
    "

    return(.self$.class == 'object')
},

isDataFrame=function() {
    ":\n\nTests if this field's type is `data.frame`.
    \nReturned value: TRUE if field's type is data frame, FALSE otherwise."

    return(.self$.class == 'data.frame')
},

isAtomic=function() {
    ":\n\nTests if this field's type is an atomic  type.
    \nReturned value: TRUE if the field's type is vector (i.e.: character,
    integer, double or logical), FALSE otherwise.
    "

    return(.self$.class %in% c('character', 'integer', 'double', 'logical'))
},

isVector=function() {
    ":\n\nTests if this field's type is a basic vector type.
    \nReturned value: TRUE if the field's type is vector (i.e.: character,
    integer, double or logical), FALSE otherwise.
    "

    lifecycle::deprecate_soft('1.0.0', 'isVector()', 'isAtomic()')

    return(.self$isAtomic())
},

equals=function(other, fail=FALSE) {
    ":\n\nCompares this instance with another, and tests if they are equal.
    \nother: Another BiodbEntryField instance.
    \nfail: If set to TRUE, then throws error instead of returning FALSE.
    \nReturned value: TRUE if they are equal, FALSE otherwise.
    "

    if ( ! methods::is(other, "BiodbEntryField"))
        error("Parameter `other` must be an instance of BiodbEntryField.")

    eq <- TRUE

    # Fields to test
    fields <- c('name', 'type', 'class', 'cardinality', 'forbids.duplicates',
                'allowed.values', 'lower.case', 'case.insensitive')

    # Loop on all fields
    for (f in fields) {
        a <- .self[[f]]
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

updateWithValuesFrom=function(other) {
    ":\n\nUpdates fields using values from `other` instance. The updated fields
    are: 'alias' and 'computable.from'. No values will be removed from those
    vectors. The new values will only be appended. This allows to extend an
    existing field inside a new connector definition.
    \nother: Another BiodbEntryField instance.
    \nReturned value: None.
    "

    if ( ! methods::is(other, "BiodbEntryField"))
        error("Parameter `other` must be an instance of BiodbEntryField.")

    # Update fields
    for (a in other$.alias)
        .self$addAlias(a)
    if ( ! is.null(other$.computable.from))
        for (directive in other$.computable.from)
            .self$addComputableFrom(directive)

    invisible(NULL)
},

show=function() {
    ":\n\nPrint informations about this entry.
    \nReturned value: None.
    "

    cat("Entry field \"", .self$.name, "\".\n", sep='')
    cat("  Description: ", .self$.description, "\n", sep='')
    cat("  Class: ", .self$.class, ".\n", sep='')
    if (.self$virtual) {
        cat("Virtual.")
        if ( ! is.null(.self$virtualGroupByType))
            cat('Grouped by type "', .self$virtualGroupByType, '".')
        cat("\n")
    }
    if (.self$.class == 'character') {
        case <-  if (.self$.case.insensitive) 'insensitive' else 'sensitive'
        lower <- if (.self$.lower.case) ' Value will be forced to lower case.'
            else ''
        cat("  Case: ", case, '.', lower, "\n", sep='')
    }
    if ( ! is.na(.self$.type))
        cat("  Type: ", .self$.type, ".\n", sep='')
    cat("  Cardinality: ", .self$.cardinality, ".\n", sep='')
    if (.self$.cardinality == 'many')
        cat("  Duplicates: ", if (.self$.forbids.duplicates) 'forbidden' else
            'allowed', ".\n", sep='')
    cat("  Aliases: ", paste(.self$.alias, collapse=', '), ".\n", sep='')
    if ( ! is.null(.self$.allowed.values))
        cat("  Allowed values: ", paste(.self$.allowed.values, collapse=', '),
            ".\n", sep='')
},

getCardinality=function() {
    return(.self$.cardinality)
},

.check=function() {
    
    # Check name
    if (is.null(.self$.name) || is.na(.self$.name) || .self$.name == '')
        warn("Missing name for entry field.")
    
    # Check description
    if (is.null(.self$.description) || is.na(.self$.description)
        || .self$.description == '')
        warn('Missing description for entry field "%s".', .self$.name)
}

))
