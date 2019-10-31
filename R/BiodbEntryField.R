# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbEntryField {{{1
################################################################################

# Declaration {{{2
################################################################################

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
#' group: The group of the field. For now only one group exists: "peak".
#' Optional.
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
        .group='character',
        .class='character',
        .cardinality='character',
        .forbids.duplicates='logical',
        .description='character',
        .alias='character',
        .allowed.values="ANY",
        .lower.case='logical',
        .case.insensitive='logical',
        .computable.from='character'
        ),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(name, alias=NA_character_, type=NA_character_,
                    group=NA_character_,
                    class=c('character', 'integer', 'double', 'logical',
                            'object', 'data.frame'), card=c('one', 'many'),
                    forbids.duplicates=FALSE, description=NA_character_,
                    allowed.values=NULL, lower.case=FALSE,
                    case.insensitive=FALSE, computable.from=NULL, ...) {

    callSuper(...)

    # Set name
    if ( is.null(name) || is.na(name) || nchar(name) == '')
        .self$error("You cannot set an empty name for a field. Name was',
                    ' empty (either NULL or NA or empty string).")
    .self$.name <- tolower(name)

    # Set type
    if ( ! is.na(type) && ! type %in% c('mass', 'name', 'id'))
        .self$error("Unknown type \"", type, "\" for field \"", name, "\".")
    .self$.type <- type

    # Set group
    if ( ! is.na(group) && ! group %in% c('peak'))
        .self$error("Unknown group \"", group, "\" for field \"", name, "\".")
    .self$.group <- group

    # Set class
    class <- match.arg(class)
    .self$.class <- class

    # Set cardinality
    card <- match.arg(card)
    .self$.cardinality <- card

    # Set description
    if (is.null(description) || is.na(description))
        .self$caution("Missing description for entry field \"", name, "\".")
    .self$.description <- description

    # Set alias
    if (length(alias) > 1 && any(is.na(alias)))
        .self$error("One of the aliases of entry field \"", name, "\" is NA.")
    .self$.alias <- alias

    # Set allowed values
    if ( ! is.null(allowed.values)) {
        if ( ! is.vector(allowed.values, mode='numeric')
            && ! is.vector(allowed.values, mode='character')
            && ! is.vector(allowed.values, mode='list'))
            .self$error('Allowed values must be either a list, a numeric',
                        ' vector or a character vector.')

        # For a list check that all values are character vectors
        if (is.vector(allowed.values, mode='list')) {
            if (is.null(names(allowed.values)))
                .self$error('When allowed values are specified as a list,',
                            ' names must be set.')
            if ( ! all(vapply(allowed.values,
                              function(x) is.vector(x, 'character'),
                              FUN.VALUE=TRUE)))
                .self$error('When allowed values are specified as a list,',
                            ' all values must be characters.')
        }
    }
    .self$.allowed.values <- allowed.values

    # Case insensitive
    if (case.insensitive && class != 'character')
        .self$error('Only character fields can be case insensitive.')
    .self$.case.insensitive <- case.insensitive

    # Lower case
    if (lower.case && class != 'character')
        .self$error('Only character fields can be forced to lower case.')
    .self$.lower.case <- lower.case

    # Computable from
    .self$.computable.from <- if (is.null(computable.from)) character()
        else computable.from

    # Set other fields
    .self$.forbids.duplicates <- forbids.duplicates
},

# Get name {{{3
################################################################################

getName=function() {
    ":\n\nGets the name.
    \nReturned value: The name of this field.
    "

    return(.self$.name)
},

# Get type {{{3
################################################################################

getType=function() {
    ":\n\nGets field's type.
    \nReturned value: The type of this field.
    "

    return(.self$.type)
},

# Get group {{{3
################################################################################

getGroup=function() {
    ":\n\nGet field's group.
    \nReturned value: The group of this field.
    "

    return(.self$.group)
},

# Get description {{{3
################################################################################

getDescription=function() {
    ":\n\nGet field's description.
    \nReturned value: The description of this field.
    "

    return(.self$.description)
},

# Has aliases {{{3
################################################################################

hasAliases=function() {
    ":\n\nTests if this field has aliases.
    \nReturned value: TRUE if this entry field defines aliases, FALSE otherwise.
    "

    return( ! any(is.na(.self$.alias)))
},

# Get aliases {{{3
################################################################################

getAliases=function() {
    ":\n\nGet aliases.
    \nReturned value: The list of aliases if some are defined, otherwise returns
    NULL."

    aliases <- NULL

    if (.self$hasAliases())
        aliases <- .self$.alias
    
    return(aliases)
},

# Get all names {{{3
################################################################################

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

# Get computable from {{{3
################################################################################

getComputableFrom=function() {
    ":\n\nGet the ID of the database from which this field can be computed.
    \nReturned value: The list of databases where to find this field's value.
    "

    return(.self$.computable.from)
},

# Correct value {{{3
################################################################################

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

# Is enumerate {{{3
################################################################################

isEnumerate=function() {
    ":\n\nTests if this field is an enumerate type (i.e.: it defines allowed
    values).
    \nReturned value: TRUE if this field defines some allowed values, FALSE
    otherwise.
    "
    
    return( ! is.null(.self$.allowed.values))
},

# Get allowed values {{{3
################################################################################

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

# Add allowed value {{{3
################################################################################

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
        .self$error('Field "', .self$.name,
                    '" doesn\'t use keys for its allowed values.')
    if ( ! key %in% names(.self$.allowed.values))
        .self$error('Field "', .self$.name, '" doesn\'t use key "', key,
                    '" for its allowed values.')

    # Check that value is not already used
    if (value %in% .self$getAllowedValues()) {
        current.key <- .self$correctValue(value)
        if (current.key != key)
            .self$error('Field "', .self$.name, '" already uses value "', value,
                        '" for its allowed values, but with key "', current.key,
                        '" instead of key "', key, '".')
        else
            .self$info('Field "', .self$.name, '" already uses value "', value,
                       '" for its allowed values, with key "', key, '".')
    }

    # Add new value
    .self$.allowed.values[[key]] <- c(.self$.allowed.values[[key]], value)
},

# Check value {{{3
################################################################################

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
        .self$error('Value(s) ', bv, ' is/are not allowed for field ',
                    .self$getName(), '. Allowed values are: ', av, '.')
    }
},

# Has card one {{{3
################################################################################

hasCardOne=function() {
    ":\n\nTests if this field has a cardinality of one.
    \nReturned value: TRUE if the cardinality of this field is one, FALSE
    otherwise.
    "

    return(.self$.cardinality == 'one')
},

# Has card many {{{3
################################################################################

hasCardMany=function() {
    ":\n\nTests if this field has a cardinality greater than one.
    \nReturned value: TRUE if the cardinality of this field is many, FALSE
    otherwise.
    "

    return(.self$.cardinality == 'many')
},

# Forbids duplicates {{{3
################################################################################

forbidsDuplicates=function() {
    ":\n\nTests if this field forbids duplicates.
    \nReturned value: TRUE if this field forbids duplicated values, FALSE
    otherwise.
    "

    return(.self$.forbids.duplicates)
},

# Is case insensitive {{{3
################################################################################

isCaseInsensitive=function() {
    ":\n\nTests if this field is case sensitive.
    \nReturned value: TRUE if this field is case insensitive, FALSE otherwise.
    "

    return(.self$.case.insensitive)
},

# Get class {{{3
################################################################################

getClass=function() {
    ":\n\nGets the class of this field's value.
    \nReturned value: The type (i.e.: class) of this field.
    "

    return(.self$.class)
},

# Is object {{{3
################################################################################

isObject=function() {
    ":\n\nTests if this field's type is a class.
    \nReturned value: TRUE if field's type is a class, FALSE otherwise.
    "

    return(.self$.class == 'object')
},

# Is data frame {{{3
################################################################################

isDataFrame=function() {
    ":\n\nTests if this field's type is `data.frame`.
    \nReturned value: TRUE if field's type is data frame, FALSE otherwise."

    return(.self$.class == 'data.frame')
},

# Is vector {{{3 
################################################################################

isVector=function() {
    ":\n\nTests if this field's type is a basic vector type.
    \nReturned value: TRUE if the field's type is vector (i.e.: character,
    integer, double or logical), FALSE otherwise.
    "

    return(.self$.class %in% c('character', 'integer', 'double', 'logical'))
},

# Equals {{{3
################################################################################

equals=function(other) {
    ":\n\nCompares this instance with another, and tests if they are equal.
    \nother: Another BiodbEntryField instance.
    \nReturned value: TRUE if they are equal, FALSE otherwise.
    "

    eq <- TRUE

    # Fields to test
    fields <- c('name', 'type', 'group', 'class', 'cardinality',
                'forbids.duplicates', 'description', 'alias', 'allowed.values',
                'lower.case', 'case.insensitive', 'computable.from')

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

    return(eq)
},

# Show {{{3 
################################################################################

show=function() {
    ":\n\nPrint informations about this entry.
    \nReturned value: None.
    "

    cat("Entry field \"", .self$.name, "\".\n", sep='')
},

# Deprecated methods {{{2
################################################################################

# Get cardinality {{{3
################################################################################

getCardinality=function() {
    .self$.deprecatedMethod('hasCardOne() or hasCardMany()')
    return(.self$.cardinality)
}

))
