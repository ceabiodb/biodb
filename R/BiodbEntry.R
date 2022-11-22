#' The mother abstract class of all database entry classes.
#'
#' An entry is an element of a database, identifiable by its accession number.
#' Each contains a list of fields defined by a name and a value. The details of
#' all fields that can be set into an entry are defined inside the class
#' \code{BiodbEntryFields}. From this class are derived other abstract classes
#' for different types of entry contents: \code{BiodbTxtEntry},
#' \code{BiodbXmlEntry}, \code{BiodbCsvEntry}, \code{BiodbJsonEntry} and
#' \code{BiodbHtmlEntry}. Then concrete classes are derived for each database:
#' \code{CompCsvEntry}, \code{MassCsvEntry}, etc. For biodb users, there is no
#' need to know this hierarchy; the knowledge of this class and its methods is
#' sufficient.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{BiodbConn}},
#' \code{\link{BiodbEntryFields}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Get a compound CSV file database
#' chebi.tsv <- system.file("extdata", "chebi_extract.tsv", package='biodb')
#'
#' # Get the connector of a compound database
#' conn <- mybiodb$getFactory()$createConn('comp.csv.file', url=chebi.tsv)
#'
#' # Get an entry:
#' entry <- conn$getEntry(conn$getEntryIds(1))
#'
#' # Get all defined fields:
#' entry$getFieldNames()
#'
#' # Get a field value:
#' accession <- entry$getFieldValue('accession')
#'
#' # Test if a field is defined:
#' if (entry$hasField('name'))
#'   print(paste("The entry's name is ", entry$getFieldValue('name'),
#'   '.', sep=''))
#'
#' # Export an entry as a data frame:
#' df <- entry$getFieldsAsDataframe()
#'
#' # You can set or reset a field's value:
#' entry$setFieldValue('mass', 1893.1883)
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import R6
#' @import lifecycle
#' @export
BiodbEntry <- R6::R6Class("BiodbEntry",

public=list(

#' @description
#' New instance initializer. Entry objects must not be created directly.
#' Instead, they are retrieved through the connector instances.
#' @param parent A valid BiodbConn instance.
#' @return Nothing.
initialize=function(parent) {

    abstractClass('BiodbEntry', self)

    private$setParent(parent)
    private$fields <- list()
    private$new <- FALSE

    return(invisible(NULL))
},

#' @description
#' Tests if the parent of this entry is a connector instance.
#' @return TRUE if this entry belongs to a connector, FALSE otherwise.
parentIsAConnector=function() {

    return(methods::is(private$parent, "BiodbConn"))
},

#' @description
#' Returns the parent instance (A BiodbConn or BiodbFactory object) to which
#' this object is attached.
#' @return A BiodbConn instance or a BiodbFactory object.
getParent=function() {

    return(private$parent)
},

#' @description
#' Returns the biodb main class instance to which this object is
#'     attached.
#' @return The main biodb instance.
getBiodb=function() {

    return(private$parent$getBiodb())
},

#' @description
#' Clones this entry.
#' @param db.class The database class (the Biodb database ID) of the
#' clone. By setting this parameter, you can specify a different database
#' for the clone, so you may clone an entry into another database if you
#' wish. By default the class of the clone will be the same as the
#' original entry.
#' @return The clone, as a new BiodbEntry instance.
cloneInstance=function(db.class=NULL) {
# TODO IMPORTANT See if we can use reserved R6 clone() method instead.

    # Create new entry
    cl <- if (is.null(db.class)) self$getDbClass() else db.class
    clone <- self$getBiodb()$getFactory()$createNewEntry(db.class=cl)

    # Copy fields
    clone$.__enclos_env__$private$fields <- private$fields

    return(clone)
},

#' @description
#' Gets the entry ID.
#' @return the entry ID, which is the value if the `accession` field.
getId=function() {

    return(self$getFieldValue('accession'))
},

#' @description
#' Tests if this entry is new.
#' @return TRUE if this entry was newly created, FALSE otherwise.
isNew=function() {

    return(private$new)
},

#' @description
#' Gets the ID of the database associated with this entry.
#' @return The name of the database class associated with this entry.
getDbClass=function() {

    # Get class name
    s <- class(self)[[1]]

    # Get connection class name
    indices <- as.integer(gregexpr('[A-Z]', s, perl=TRUE)[[1]])

    # Add dots
    last.word <- TRUE
    for (i in rev(indices))
        if (last.word) {
            # We cut last word which should be "Entry"
            s <- substring(s, 1, i - 1)
            last.word <- FALSE
        }
        else if (i != 1)
            s <- paste(substring(s, 1, i - 1), '.', substring(s, i), sep='')

    # Set to lowercase
    s <- tolower(s)

    return(s)
},

#' @description
#' Sets the value of a field. If the field is not already set for this
#'     entry, then the field will be created. See BiodbEntryFields for a list of
#'     possible fields in biodb.
#' @param field The name of a field.
#' @param value The value to set.
#' @return Nothing.
setFieldValue=function(field, value) {

    field.def <- self$getBiodb()$getEntryFields()$get(field)
    field <- field.def$getName()

    # Specific case to handle objects.
    if (field.def$isObject() && !(isS4(value) & methods::is(value, "refClass")))
        error0('Cannot set a non RC instance to field "', field,
        '" in BiodEntry.')

    # Check value class
    if (field.def$isAtomic()) {
        if (length(value) == 0)
            error0('Cannot set an empty value into field "', field, '".')
        v <- as.vector(value, mode=field.def$getClass())
        if ( ! all(is.na(value)) && all(is.na(v)))
            warn0("Unable to convert value(s) \"",
            paste(value, collapse=', '), "\" into ",
            field.def$getClass(), " type for field \"", field,
            "\".")
        value <- v
    }

    # Check value
    field.def$checkValue(value)

    # Correct value
    value <- field.def$correctValue(value)

    # Remove duplicates
    if (field.def$forbidsDuplicates() || (field.def$isAtomic()
        && field.def$hasCardOne()))
        value <- value[ ! duplicated(if (field.def$isCaseInsensitive())
            tolower(value) else value)]

    # Check cardinality
    if ( ! field.def$isDataFrame() && field.def$hasCardOne()) {
        if (length(value) > 1)
            error0('Cannot set more that one value (',
            paste(value, collapse=', '),
            ') into single value field "', field, '" for entry ',
            self$getName(), '.')
        if (length(value) == 0)
            error0('Cannot set an empty vector into single value field "',
            field, '" for entry ', self$getName(), '.')
    }

    # Set value
    private$fields[[field.def$getName()]] <- value

    return(invisible(NULL))
},

#' @description
#' Appends a value to an existing field. If the field is not defined for
#'     this entry, then the field will be created and set to this value. Only
#'     fields with a cardinality greater than one can accept multiple values.
#' @param field The name of a field.
#' @param value The value to append.
#' @return Nothing.
appendFieldValue=function(field, value) {

    if (self$hasField(field))
        self$setFieldValue(field, c(self$getFieldValue(field), value))
    else
        self$setFieldValue(field, value)

    return(invisible(NULL))
},

#' @description
#' Gets a list of all fields defined for this entry.
#' @return A character vector containing all field names defined in
#'     this entry.
getFieldNames=function() {

    return(sort(names(private$fields)))
},

#' @description
#' Tests if a field is defined in this entry.
#' @param field The name of a field.
#' @return TRUE if the specified field is defined in this entry,
#'     FALSE otherwise.
hasField=function(field) {

    # Get field definition
    field.def <- self$getBiodb()$getEntryFields()$get(field)
    field <- field.def$getName()

    return(tolower(field) %in% names(private$fields))
},

#' @description
#' Removes the specified field from this entry.
#' @param field The name of a field.
#' @return Nothing.
removeField=function(field) {

    if (self$hasField(field))
        private$fields <- private$fields[names(private$fields)
            != tolower(field)]

    return(invisible(NULL))
},

#' @description
#' Gets the value of the specified field.
#' @param field The name of a field.
#' @param compute If set to TRUE and a field is not defined, try to compute it
#' using internal defined computing rules. If set to FALSE, let the field
#' undefined.
#' @param flatten If set to TRUE and a field's value is a vector of more than
#' one element, then export the field's value as a single string composed of
#' the field's value concatenated and separated by the character defined in the
#' 'multival.field.sep' config key. If set to FALSE or the field contains only
#' one value, changes nothing.
#' @param last If set to TRUE and a field's value is a vector of more than one
#' element, then export only the last value. If set to FALSE, changes nothing.
#' @param limit The maximum number of values to get in case the field contains
#' more than one value.
#' @param withNa If set to TRUE, keep NA values. Otherwise filter out NAs
#' values in vectors.
#' @param duplicatedValues If set to TRUE, keeps duplicated values.
#' @return The value of the field.
getFieldValue=function(field, compute=TRUE, flatten=FALSE, last=FALSE, limit=0,
    withNa=TRUE, duplicatedValues=TRUE) {

    val <- NULL
    field <- tolower(field)
    cfg <- self$getBiodb()$getConfig()

    # Get field definition
    field.def <- self$getBiodb()$getEntryFields()$get(field)
    field <- field.def$getName()

    # Compute field value
    if (compute && ! self$hasField(field) && ! field.def$isVirtual())
        self$computeFields(field)

    # Get value for real field
    if (self$hasField(field))
        val <- private$fields[[field]]

    # Get value of virtual field
    else if (field.def$isVirtual()) {
        gbt <- field.def$getVirtualGroupByType()
        # Gather other fields to build data frame
        if (field.def$isDataFrame() && ! is.null(gbt))
            val <- self$getFieldsAsDataframe(fields.type=gbt,
                flatten=FALSE, duplicate.rows=FALSE, only.atomic=FALSE,
                sort=TRUE)

        else
            error0('Do not know how to compute virtual field "', field,
                '" for entry "', self$getFieldValue('accession'), '".')
    }

    # Unset field
    else {
        # Return NULL or NA
        val <- if (field.def$isAtomic() && field.def$hasCardOne())
            as.vector(NA, mode=field.def$getClass()) else NULL
    }

    # Get last value only
    if (last && field.def$hasCardMany() && length(val) > 1)
        val <- val[[length(val)]]

    # Remove NA values
    if ( ! withNa && ! is.null(val) && length(val) > 0)
        val <- val[ ! is.na(val)]

    # Limit
    if (limit > 0 && ! is.null(val) && length(val) > limit)
        val <- val[seq(limit)]

    # Remove duplicated values
    if ( ! duplicatedValues)
        val <- val[ ! duplicated(val)]

    # Flatten: convert atomic values with cardinality > 1 into a string
    if (flatten && ! is.null(val)) {
        if (field.def$isAtomic() && field.def$hasCardMany()
            && length(val) > 1) {
            if (all(is.na(val)))
                val <-  as.vector(NA, mode=field.def$getClass())
            else
                val <- paste(val, collapse=cfg$get('multival.field.sep'))
        }
    }

    return(val)
},

#' @description
#' Gets the fields of this entry that have the specified type.
#' @param type The type of fields to retrieve.
#' @return A character vector containing the field names.
getFieldsByType=function(type) {
    ef <- self$getBiodb()$getEntryFields()
    fct <- function(f) { ef$get(f)$getType() == type }
    fields <- Filter(fct, names(private$fields))

    return(fields)
},

#' @description
#' Converts this entry into a data frame.
#' @param only.atomic If set to TRUE, only export field's values that are atomic
#' @param (i.e. of type vector).
#' @param compute If set to TRUE and a field is not defined, try to compute it
#' using internal defined computing rules. If set to FALSE, let the field
#' undefined.
#' @param fields Set to character vector of field names in order to restrict
#' execution to this set of fields.
#' @param fields.type If set, output all the fields of the specified type.
#' @param flatten If set to TRUE and a field's value is a vector of more than
#' one element, then export the field's value as a single string composed of
#' the field's value concatenated and separated by the character defined in the
#' 'multival.field.sep' config key. If set to FALSE or the field contains only
#' one value, changes nothing.
#' @param limit The maximum number of field values to write into new columns.
#' Used for fields that can contain more than one value.
#' @param only.card.one If set to TRUE, only fields with a cardinality of one
#' will be extracted.
#' @param own.id If set to TRUE includes the database id field named
#' `<database_name>.id` whose values are the same as the `accession` field.
#' @param duplicate.rows If set to TRUE and merging field values with
#' cardinality greater than one, values will be duplicated.
#' @param sort If set to TRUE sort the order of columns alphabetically,
#' otherwise do not sort.
#' @param virtualFields If set to TRUE includes also virtual fields, otherwise
#' excludes them.
#' @return A data frame containg the values of the fields.
getFieldsAsDataframe=function(only.atomic=TRUE, compute=TRUE, fields=NULL,
    fields.type=NULL, flatten=TRUE, limit=0, only.card.one=FALSE, own.id=TRUE,
    duplicate.rows=TRUE, sort=FALSE, virtualFields=FALSE) {

    if ( ! is.null(fields))
        fields <- tolower(fields)

    # Compute fields
    if (compute)
        self$computeFields(fields)

    # Select fields
    fields <- private$selectFields(fields=fields, fields.type=fields.type,
        own.id=own.id, only.atomic=only.atomic, only.card.one=only.card.one)

    # Organize fields by groups
    groups <- private$organizeFieldsByGroups(fields)

    # Process data frame groups
    fct <-  function(fields) {
        private$fieldsToDataframe(fields, flatten=flatten, duplicate.rows=FALSE,
            limit=limit)
    }
    groupsDf <- lapply(groups$dfGrps, fct)

    # Process single fields
    singlesDf <- private$fieldsToDataframe(groups$singles,
        duplicate.rows=duplicate.rows, flatten=flatten, limit=limit,
        duplicatedValues=FALSE)

    # Merge all data frames
    outdf <- private$mergeDataframes(c(list(singlesDf), groupsDf),
        duplicate.rows=duplicate.rows)

    # Sort
    if (sort)
        outdf <- outdf[, sort(names(outdf))]

    return(outdf)
},

#' @description
#' Converts this entry into a JSON string.
#' @param compute If set to TRUE and a field is not defined, try to compute it
#'     using internal defined computing rules. If set to FALSE, let the field
#'     undefined.
#' @return A JSON object from jsonlite package.
getFieldsAsJson=function(compute=TRUE) {

    # Compute fields
    if (compute)
        self$computeFields()

    return(jsonlite::toJSON(private$fields, pretty=TRUE, digits=NA_integer_,
        auto_unbox=TRUE))
},

#' @description
#' Parses content string and set values accordingly for this entry's
#' fields.  This method is called automatically and should be run directly
#' by users.
#' @param content A character string containing definition for an entry and
#' @param obtained from a database. The format can be CSV, HTML, JSON,
#' XML, or just text.
#' @return Nothing.
parseContent=function(content) {

    # No connector?
    if ( ! self$parentIsAConnector())
        error0('Impossible to parse content for this entry, because its',
            ' parent is not a connector.')

    # Parse
    if (private$isContentCorrect(content)) {

        # Parse content
        parsed.content <- private$doParseContent(content)

        if (private$isParsedContentCorrect(parsed.content)) {

            private$doParseFieldsStep1(parsed.content)

            private$doParseFieldsStep2(parsed.content)
        }
    }

    # Make sure the database id field is set to the same value as the accession
    # field
    # TODO Factorize this test in a separate method.
    dbid.field <- private$parent$getEntryIdField()
    if (self$hasField(dbid.field) && self$hasField('accession')) {
        if (self$getFieldValue('accession') != self$getFieldValue(dbid.field))
            error0('Value of accession field ("',
                self$getFieldValue('accession'),
                '") is different from value of ', dbid.field,
                ' field ("', self$getFieldValue(dbid.field), '").')
    }
    else {
        if (self$hasField(dbid.field))
            self$setFieldValue('accession', self$getFieldValue(dbid.field))
        else if (self$hasField('accession'))
            self$setFieldValue(dbid.field, self$getFieldValue('accession'))
    }

    return(invisible(NULL))
},

#' @description
#' Computes fields. Look at all missing fields, and try to compute them
#'     using references to other databases, if a rule exists.
#' @param fields A list of fields to review for computing. By default all fields
#'     will be reviewed.
#' @return TRUE if at least one field was computed successfully,
#'     FALSE otherwise.
computeFields=function(fields=NULL) {

    success <- FALSE
    if ( ! is.null(fields))
        fields <- tolower(fields)

    if (self$getBiodb()$getConfig()$isEnabled('compute.fields')) {

        # Set of fields to compute
        if (is.null(fields)) {
            ef <- self$getBiodb()$getEntryFields()
            fields <- ef$getFieldNames(computable=TRUE)
        }

        # Loop on all fields
        for(f in fields) {
            s <- private$computeField(f)
            success <- success || s
        }
    }

    return(success)
},

#' @description
#' Displays short information about this instance.
#' @return Nothing.
print=function() {

    db <- private$parent$getPropertyValue('name')
    id <- self$getFieldValue('accession', compute=FALSE)
    id <- if (is.na(id)) 'ID unknown' else id
    cat("Biodb ", db, " entry instance ", id, ".\n", sep='')

    return(invisible(NULL))
},

#' @description
#' Gets a short text describing this entry instance.
#' @return A character value concatenating the connector name with
#'     the entry accession.
getName=function() {

    name <- paste(private$parent$getPropertyValue('name'),
            self$getFieldValue('accession'))

    return(name)
},

#' @description
#' Tests if this entry makes reference to another entry.
#' @param db Another database connector.
#' @param oid A entry ID from database db.
#' @param recurse If set to TRUE, the algorithm will follow all references to
#' entries from other databases, to see if it can establish an indirect link to
#' `oid`.
#' @return TRUE if this entry makes reference to the entry oid from database
#' db, FALSE otherwise.
makesRefToEntry=function(db, oid, recurse=FALSE) {

    makes_ref <- FALSE
    field <- paste(db, 'id', sep='.')

    # Check if oid is inside field
    if (self$hasField(field) && oid %in% self$getFieldValue(field))
        makes_ref <- TRUE

    # Recursive search
    else if (recurse)
        makes_ref <- private$makesRefToEntryRecurse(db, oid)
    # TODO Why not describe the recurse tree to follow in each DbInfo object?
    # Specialy if we use JSON to register/define DbInfo objects.

    return(makes_ref)
},

#' @description
#' DEPRECATED. Gets the value of a field.
#' @param field The name of the field.
#' @return The value of the field.
getField=function(field) {
    lifecycle::deprecate_soft('1.0.0', 'getField()', "getFieldValue()")
    return(self$getFieldValue(field))
},

#' @description
#' DEPRECATED. Sets the value of a field.
#' @param field The name of the field.
#' @param value The new value of the field. 
#' @return Nothing.
setField=function(field, value) {
    lifecycle::deprecate_warn('1.0.0', 'setField()', "setFieldValue()")
    self$setFieldValue(field, value)
    return(invisible(NULL))
},

#' @description
#' Gets the class of a field.
#' @param field The name of the field.
#' @return The class of the field.
getFieldClass=function(field) {
    return(self$getBiodb()$getEntryFields()$get(field)$getClass())
},

#' @description
#' Gets the definition of an entry field.
#' @param field The name of the field.
#' @return An object BiodbEntryField which defines the field.
getFieldDef=function(field) {
    return(self$getBiodb()$getEntryFields()$get(field))
},

#' @description
#' Gets the cardinality of the field.
#' @param field The name of the field.
#' @return The cardinality of the field.
getFieldCardinality=function(field) {
    return(self$getFieldDef(field)$getCardinality())
},

#' @description
#' DEPRECATED. Use BiodbEntryField::isVector() instead.
#' @param field The name of the field.
#' @return TRUE if the field as a basic type (logical, numeric, character, ...).
fieldHasBasicClass=function(field) {

    lifecycle::deprecate_warn('1.0.0', 'fieldHasBasicClass()',
        'BiodbEntryField::isVector()')

    return(self$getBiodb()$getEntryFields()$get(field)$isVector())
}

),

private=list(
    fields=NULL,
    new=NULL,
    parent=NULL,

setParent=function(parent) {

    if ( ! (chk::vld_is(parent, 'BiodbConn')
        || chk::vld_is(parent, 'BiodbFactory')))
        error0("'parent' must be either a BiodbConn instance or a BiodbFactory",
            " instance.")

    private$parent <- parent
},

setAsNew=function(isNew) {
    private$new <- isNew
},

computeField=function(field) {
    # Compute one single field

    success <- FALSE

    ef <- self$getBiodb()$getEntryFields()$get(field)

    # Skip this field if we already have a value for it
    if ( ! self$hasField(field) && ef$isComputable()) {

        # Loop on all computing directives
        for (directive in ef$isComputableFrom()) {

            db <- directive$database
            value <- NULL

            # Database is itself
            if (db == 'self' || (methods::is(private$parent, 'BiodbConn')
                && db == private$parent$getId())) {
                # Look for field in entry
                if ('fields' %in% names(directive))
                    for (otherField in directive$fields)
                        if (self$hasField(otherField)) {
                            value <- self$getFieldValue(otherField)
                            break
                        }
            }

            # Look into another database
            else {
                # Have we a reference for this database?
                db.id.field <- paste(db, 'id', sep='.')
                if ( ! self$hasField(db.id.field))
                    next
                db.id <- self$getFieldValue(db.id.field, compute=FALSE)
                if ( ! is.na(db.id)) {

                    # Get value for this field in the database
                    logDebug('Compute value for field "%s".', field) 
                    db.entry <- self$getBiodb()$getFactory()$getEntry(db,
                        id=db.id)

                    # Get found value
                    if ( ! is.null(db.entry))
                        value <- db.entry$getFieldValue(field, compute=FALSE)
                }
            }

            # Set found value
            if ( ! is.null(value)) {
                self$setFieldValue(field, value)
                success <- TRUE
                break
            }
        }
    }

    return(success)
},

makesRefToEntryRecurse=function(db, oid) {
    return(FALSE)
},

isContentCorrect=function(content) {

    correct <- ! is.null(content) && ((is.list(content) && length(content) > 0)
        || (is.character(content) && ! is.na(content) && content != ''))
    # NOTE `nchar(content)` may give "invalid multibyte string, element 1" on
    # some strings.

    return(correct && private$doCheckContent(content))
}

,doCheckContent=function(content) {
    return(TRUE)
}

,doParseContent=function(content) {
    abstractMethod(self)
}

,isParsedContentCorrect=function(parsed.content) {
    return( ! is.null(parsed.content)
        && ( ! is.vector(parsed.content) || length(parsed.content) > 1
        || ! is.na(parsed.content))
        && private$doCheckParsedContent(parsed.content)
        )
}

,doCheckParsedContent=function(parsed.content) {
    return(TRUE)
}

,doParseFieldsStep1=function(parsed.content) {
    abstractMethod(self)
}

,doParseFieldsStep2=function(parsed.content) {
},

checkDbIdField=function() {
},

organizeFieldsByGroups=function(fields) {

    singles <- character()
    dfGrps <- list()
    ef <- self$getBiodb()$getEntryFields()
    logTrace('Fields %s', lst2str(fields))

    for (field in fields) {
        fieldDef <- ef$get(field)

        # Data frame groups
        dfGrp <- fieldDef$getDataFrameGroup()
        if ( ! is.na(dfGrp))
            dfGrps[[dfGrp]] <- c(dfGrps[[dfGrp]], field)

        # Single fields
        else
            singles <- c(singles, field)
    }

    # Build groups
    groups <- list(singles=singles, dfGrps=dfGrps)
    logTrace('Groups %s', lst2str(groups))

    return(groups)
},

selectFields=function(fields, fields.type, own.id, only.atomic, only.card.one)
{

    logTrace('Fields %s', lst2str(fields))
    logTrace('Fields type: %s', fields.type)

    # Set fields to get
    logTrace('Fields is null: %s', is.null(fields))
    logTrace('Fields.type is null: %s', is.null(fields.type))
    if ( ! is.null(fields.type))
        fields <- self$getFieldsByType(fields.type)
    else if (is.null(fields))
        fields <- names(private$fields)
    logTrace('Fields %s', lst2str(fields))

    # Filter out unwanted fields
    ef <- self$getBiodb()$getEntryFields()
    if ( ! own.id) {
        ownIdField <- private$parent$getEntryIdField()
        fields <- Filter(function(f) f != ownIdField, fields)
    }
    if (only.atomic)
        fields <- Filter(function(f) ef$get(f)$isAtomic(), fields)
    if (only.card.one)
        fields <- Filter(function(f) ef$get(f)$hasCardOne(), fields)
    # Ignore if value is not data frame or vector
    fields <- Filter(function(f) ef$get(f)$isAtomic() ||
        ef$get(f)$isDataFrame(), fields)
    # Keep only fields with a value
    fields <- fields[fields %in% names(private$fields)]

    logTrace('Fields %s', lst2str(fields))
    return(fields)
},

fieldsToDataframe=function(fields, duplicate.rows, flatten, limit,
                            duplicatedValues=TRUE) {

    # Transform values in data frames
    toDf <- function(f) {
        v <- self$getFieldValue(f, flatten=flatten, limit=limit,
            duplicatedValues=duplicatedValues)

        # Transform vector into data frame
        if (is.vector(v)) {
            v <- as.data.frame(v, stringsAsFactors=FALSE)
            colnames(v) <- f
        }

        return(v)
    }
    dataFrames <- lapply(fields, toDf)

    # Merge data frames
    outdf <- private$mergeDataframes(dataFrames, duplicate.rows=duplicate.rows)

    return(outdf)
},

mergeDataframes=function(dataframes, duplicate.rows) {

    outdf <- data.frame(stringsAsFactors=FALSE)

    for (v in dataframes)
        if (nrow(v) > 0) {
            if (nrow(outdf) == 0)
                outdf <- v
            else if ( ! duplicate.rows && nrow(outdf) == nrow(v))
                outdf <- cbind(outdf, v)
            else
                outdf <- merge(outdf, v)
        }

    return(outdf)
}
))
