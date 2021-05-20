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
#' @import methods
#' @import lifecycle
#' @include BiodbChildObject.R
#' @export BiodbEntry
#' @exportClass BiodbEntry
BiodbEntry <- methods::setRefClass("BiodbEntry",
    contains="BiodbChildObject",
    fields=list(
        .fields='list',
        .new='logical'
    ),

methods=list(

initialize=function(...) {

    callSuper(...)
    .self$.abstractClass('BiodbEntry')

    .self$.fields <- list()
    .self$.new <- FALSE
},

parentIsAConnector=function() {
    ":\n\nTests if the parent of this entry is a connector instance.
    \nReturned value: TRUE if this entry belongs to a connector, FALSE
    otherwise.
    "

    return(is(.self$getParent(), "BiodbConn"))
},

clone=function(db.class=NULL) {
    ":\n\nClones this entry.
    \ndb.class: The database class (the Biodb database ID) of the clone. By
    setting this parameter, you can specify a different database for the clone,
    so you may clone an entry into another database if you wish. By
    default the class of the clone will be the same as the original entry.
    \nReturned value: The clone, as a new BiodbEntry instance.
    "

    # Create new entry
    cl <- if (is.null(db.class)) .self$getDbClass() else db.class
    clone <- .self$getBiodb()$getFactory()$createNewEntry(db.class=cl)

    # Copy fields
    clone$.fields <- .self$.fields

    return(clone)
},

getId=function() {
    ":\n\nGets the entry ID.
    \nReturned value: the entry ID, which is the value if the `accession` field.
    "

    return(.self$getFieldValue('accession'))
},

isNew=function() {
    ":\n\nTests if this entry is new.
    \nReturned value: TRUE if this entry was newly created, FALSE otherwise.
    "

    return(.self$.new)
},

getDbClass=function() {
    ":\n\nGets the ID of the database associated with this entry.
    \nReturned value: The name of the database class associated with this entry.
    "

    # Get class name
    s <- class(.self)

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

setFieldValue=function(field, value) {
    ":\n\nSets the value of a field. If the field is not already set for this
    entry, then the field will be created. See BiodbEntryFields for a list of
    possible fields in biodb.
    \nfield: The name of a field.
    \nvalue: The value to set.
    \nReturned value: None.
    "

    field.def <- .self$getBiodb()$getEntryFields()$get(field)
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
            .self$getName(), '.')
        if (length(value) == 0)
            error0('Cannot set an empty vector into single value field "',
            field, '" for entry ', .self$getName(), '.')
    }

    # Set value
    .self$.fields[[field.def$getName()]] <- value
},

appendFieldValue=function(field, value) {
    ":\n\nAppends a value to an existing field. If the field is not defined for
    this entry, then the field will be created and set to this value. Only
    fields with a cardinality greater than one can accept multiple values.
    \nfield: The name of a field.
    \nvalue: The value to append.
    \nReturned value: None.
    "

    if (.self$hasField(field))
        .self$setFieldValue(field, c(.self$getFieldValue(field), value))
    else
        .self$setFieldValue(field, value)
},

getFieldNames=function() {
    ":\n\nGets a list of all fields defined for this entry.
    \nReturned value: A character vector containing all field names defined in
    this entry.
    "

    return(sort(names(.self$.fields)))
},

hasField=function(field) {
    ":\n\nTests if a field is defined in this entry.
    \nfield: The name of a field.
    \nReturned value: TRUE if the specified field is defined in this entry,
    FALSE otherwise.
    "

    # Get field definition
    field.def <- .self$getBiodb()$getEntryFields()$get(field)
    field <- field.def$getName()

    return(tolower(field) %in% names(.self$.fields))
},

removeField=function(field) {
    ":\n\nRemoves the specified field from this entry.
    \nfield: The name of a field.
    \nReturned value: None.
    "

    if (.self$hasField(field))
        .self$.fields <- .self$.fields[names(.self$.fields) != tolower(field)]
},

getFieldValue=function(field, compute=TRUE, flatten=FALSE, last=FALSE, limit=0,
    withNa=TRUE, duplicatedValues=TRUE) {
    ":\n\nGets the value of the specified field.
    \nfield: The name of a field.
    \ncompute: If set to TRUE and a field is not defined, try to compute it
    using internal defined computing rules. If set to FALSE, let the field
    undefined.
    \nflatten: If set to TRUE and a field's value is a vector of more than one
    element, then export the field's value as a single string composed of the
    field's value concatenated and separated by the character defined in the
    'multival.field.sep' config key. If set to FALSE or the field contains only
    one value, changes nothing.
    \nlast: If set to TRUE and a field's value is a vector of more than one
    element, then export only the last value. If set to FALSE, changes nothing.
    \nlimit: The maximum number of values to get in case the field contains more
    than one value.
    \nwithNa: If set to TRUE, keep NA values. Otherwise filter out NAs values in
    vectors.
    \nReturned value: The value of the field.
    "

    val <- NULL
    field <- tolower(field)
    cfg <- .self$getBiodb()$getConfig()

    # Get field definition
    field.def <- .self$getBiodb()$getEntryFields()$get(field)
    field <- field.def$getName()

    # Compute field value
    if (compute && ! .self$hasField(field) && ! field.def$isVirtual())
        .self$computeFields(field)

    # Get value for real field
    if (.self$hasField(field))
        val <- .self$.fields[[field]]

    # Get value of virtual field
    else if (field.def$isVirtual()) {
        gbt <- field.def$getVirtualGroupByType()
        # Gather other fields to build data frame
        if (field.def$isDataFrame() && ! is.null(gbt))
            val <- .self$getFieldsAsDataframe(fields.type=gbt,
                flatten=FALSE, duplicate.rows=FALSE, only.atomic=FALSE,
                sort=TRUE)

        else
            error0('Do not know how to compute virtual field "', field,
                '" for entry "', .self$getFieldValue('accession'), '".')
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

getFieldsByType=function(type) {
    ":\n\nGets the fields of this entry that have the specified type.
    \nReturned value: A character vector containing the field names."

    ef <- .self$getBiodb()$getEntryFields()
    fct <- function(f) { ef$get(f)$getType() == type }
    fields <- Filter(fct, names(.self$.fields))

    return(fields)
},

getFieldsAsDataframe=function(only.atomic=TRUE, compute=TRUE, fields=NULL,
    fields.type=NULL, flatten=TRUE, limit=0, only.card.one=FALSE, own.id=TRUE,
    duplicate.rows=TRUE, sort=FALSE, virtualFields=FALSE) {
    ":\n\nConverts this entry into a data frame.
    \nonly.atomic: If set to TRUE, only export field's values that are atomic
    (i.e.: of type vector).
    \ncompute: If set to TRUE and a field is not defined, try to compute it
    using internal defined computing rules. If set to FALSE, let the field
    undefined.
    \nfields: Set to character vector of field names in order to restrict
    execution to this set of fields.
    \nfields.type: If set, output all the fields of the specified type.
    \nflatten: If set to TRUE and a field's value is a vector of more than one
    element, then export the field's value as a single string composed of the
    field's value concatenated and separated by the character defined in the
    'multival.field.sep' config key. If set to FALSE or the field contains only
    one value, changes nothing.
    \nlimit: The maximum number of field values to write into new columns. Used
    for fields that can contain more than one value.
    \nonly.card.one: If set to TRUE, only fields with a cardinality of one will
    be extracted.
    \nown.id: If set to TRUE includes the database id field named
    `<database_name>.id` whose values are the same as the `accession` field.
    \nduplicate.rows: If set to TRUE and merging field values with cardinality
    greater than one, values will be duplicated.
    \nsort: If set to TRUE sort the order of columns alphabetically, otherwise
    do not sort.
    \nvirtualFields: If set to TRUE includes also virtual fields, otherwise
    excludes them.
    \nReturned value: A data frame containg the values of the fields.
    "

    if ( ! is.null(fields))
        fields <- tolower(fields)

    # Compute fields
    if (compute)
        .self$computeFields(fields)

    # Select fields
    fields <- .self$.selectFields(fields=fields, fields.type=fields.type,
        own.id=own.id, only.atomic=only.atomic, only.card.one=only.card.one)

    # Organize fields by groups
    groups <- .self$.organizeFieldsByGroups(fields)

    # Process data frame groups
    fct <-  function(fields) {
        .self$.fieldsToDataframe(fields, flatten=flatten, duplicate.rows=FALSE,
            limit=limit)
    }
    groupsDf <- lapply(groups$dfGrps, fct)

    # Process single fields
    singlesDf <- .self$.fieldsToDataframe(groups$singles,
        duplicate.rows=duplicate.rows, flatten=flatten, limit=limit,
        duplicatedValues=FALSE)

    # Merge all data frames
    outdf <- .self$.mergeDataframes(c(list(singlesDf), groupsDf),
        duplicate.rows=duplicate.rows)

    # Sort
    if (sort)
        outdf <- outdf[, sort(names(outdf))]

    return(outdf)
},

getFieldsAsJson=function(compute=TRUE) {
    ":\n\nConverts this entry into a JSON string.
    \ncompute: If set to TRUE and a field is not defined, try to compute it
    using internal defined computing rules. If set to FALSE, let the field
    undefined.
    \nReturned value: A JSON object from jsonlite package.
    "

    # Compute fields
    if (compute)
        .self$computeFields()

    return(jsonlite::toJSON(.self$.fields, pretty=TRUE, digits=NA_integer_,
        auto_unbox=TRUE))
},

parseContent=function(content) {
    ":\n\nParses content string and set values accordingly for this entry's
    fields.  This method is called automatically and should be run directly by
    users.
    \ncontent: A character string containing definition for an entry and
    obtained from a database. The format can be: CSV, HTML, JSON, XML, or just
    text.
    \nReturned value: None.
    "

    # No connector?
    if ( ! .self$parentIsAConnector())
        error0('Impossible to parse content for this entry, because its',
            ' parent is not a connector.')

    # Parse
    if (.self$.isContentCorrect(content)) {

        # Parse content
        parsed.content <- .self$.doParseContent(content)

        if (.self$.isParsedContentCorrect(parsed.content)) {

            .self$.parseFieldsStep1(parsed.content)

            .self$.parseFieldsStep2(parsed.content)
        }
    }

    # Make sure the database id field is set to the same value as the accession
    # field
    # TODO Factorize this test in a separate method.
    dbid.field <- .self$getParent()$getEntryIdField()
    if (.self$hasField(dbid.field) && .self$hasField('accession')) {
        if (.self$getFieldValue('accession') != .self$getFieldValue(dbid.field))
            error0('Value of accession field ("',
                .self$getFieldValue('accession'),
                '") is different from value of ', dbid.field,
                ' field ("', .self$getFieldValue(dbid.field), '").')
    }
    else {
        if (.self$hasField(dbid.field))
            .self$setFieldValue('accession', .self$getFieldValue(dbid.field))
        else if (.self$hasField('accession'))
            .self$setFieldValue(dbid.field, .self$getFieldValue('accession'))
    }
},

.computeField=function(field) {
    # Compute one single field

    success <- FALSE

    ef <- .self$getBiodb()$getEntryFields()$get(field)

    # Skip this field if we already have a value for it
    if ( ! .self$hasField(field) && ef$isComputable()) {

        # Loop on all computing directives
        for (directive in ef$isComputableFrom()) {

            db <- directive$database
            value <- NULL

            # Database is itself
            if (db == 'self' || (methods::is(.self$getParent(), 'BiodbConn')
                && db == .self$getParent()$getId())) {
                # Look for field in entry
                if ('fields' %in% names(directive))
                    for (otherField in directive$fields)
                        if (.self$hasField(otherField)) {
                            value <- .self$getFieldValue(otherField)
                            break
                        }
            }

            # Look into another database
            else {
                # Have we a reference for this database?
                db.id.field <- paste(db, 'id', sep='.')
                if ( ! .self$hasField(db.id.field))
                    next
                db.id <- .self$getFieldValue(db.id.field, compute=FALSE)
                if ( ! is.na(db.id)) {

                    # Get value for this field in the database
                    logDebug('Compute value for field "%s".', field) 
                    db.entry <- .self$getBiodb()$getFactory()$getEntry(db,
                        id=db.id)

                    # Get found value
                    if ( ! is.null(db.entry))
                        value <- db.entry$getFieldValue(field, compute=FALSE)
                }
            }

            # Set found value
            if ( ! is.null(value)) {
                .self$setFieldValue(field, value)
                success <- TRUE
                break
            }
        }
    }

    return(success)
},

computeFields=function(fields=NULL) {
    ":\n\nComputes fields. Look at all missing fields, and try to compute them
    using references to other databases, if a rule exists.
    \nfields: A list of fields to review for computing. By default all fields
    will be reviewed.
    \nReturned value: TRUE if at least one field was computed successfully,
    FALSE otherwise.
    "

    success <- FALSE
    if ( ! is.null(fields))
        fields <- tolower(fields)

    if (.self$getBiodb()$getConfig()$isEnabled('compute.fields')) {

        # Set of fields to compute
        if (is.null(fields)) {
            ef <- .self$getBiodb()$getEntryFields()
            fields <- ef$getFieldNames(computable=TRUE)
        }

        # Loop on all fields
        for(f in fields) {
            s <- .self$.computeField(f)
            success <- success || s
        }
    }

    return(success)
},

show=function() {
    ":\n\nDisplays short information about this instance.
    \nReturned value: None.
    "

    db <- .self$getParent()$getPropertyValue('name')
    id <- .self$getFieldValue('accession', compute=FALSE)
    id <- if (is.na(id)) 'ID unknown' else id
    cat("Biodb ", db, " entry instance ", id, ".\n", sep='')
},

getName=function() {
    ":\n\nGets a short text describing this entry instance.
    \nReturned value: A character value concatenating the connector name with
    the entry accession.
    "

    name <- paste(.self$getParent()$getName(), .self$getFieldValue('accession'))

    return(name)
},

makesRefToEntry=function(db, oid, recurse=FALSE) {
    ":\n\nTests if this entry makes reference to another entry.
    \ndb: Another database connector.
    \noid: A entry ID from database db.
    \nrecurse: If set to TRUE, the algorithm will follow all references to
    entries from other databases, to see if it can establish an indirect link
    to `oid`.
    \nReturned value: TRUE if this entry makes reference to the entry oid from
    database db, FALSE otherwise.
    "

    makes_ref <- FALSE
    field <- paste(db, 'id', sep='.')

    # Check if oid is inside field
    if (.self$hasField(field) && oid %in% .self$getFieldValue(field))
        makes_ref <- TRUE

    # Recursive search
    else if (recurse)
        makes_ref <- .self$.makesRefToEntryRecurse(db, oid)
    # TODO Why not describe the recurse tree to follow in each DbInfo object?
    # Specialy if we use JSON to register/define DbInfo objects.

    return(makes_ref)
},

.makesRefToEntryRecurse=function(db, oid) {
    return(FALSE)
},

.setAsNew=function(new) {
    .self$.new <- new
},

.isContentCorrect=function(content) {

    correct <- ! is.null(content) && ((is.list(content) && length(content) > 0)
        || (is.character(content) && ! is.na(content) && content != ''))
    # NOTE `nchar(content)` may give "invalid multibyte string, element 1" on
    # some strings.

    return(correct)
},

.doParseContent=function(content) {
    .self$.abstractMethod()
},

.isParsedContentCorrect=function(parsed.content) {
    return( ! is.null(parsed.content)
        && ( ! is.vector(parsed.content) || length(parsed.content) > 1
        || ! is.na(parsed.content)))
},

.parseFieldsStep1=function(parsed.content) {
    .self$.abstractMethod()
},

.parseFieldsStep2=function(parsed.content) {
},

.checkDbIdField=function() {
},

getField=function(field) {
    lifecycle::deprecate_soft('1.0.0', 'getField()', "getFieldValue()")
    return(.self$getFieldValue(field))
},

setField=function(field, value) {
    lifecycle::deprecate_warn('1.0.0', 'setField()', "setFieldValue()")
    .self$setFieldValue(field, value)
},

getFieldClass=function(field) {

    return(.self$getBiodb()$getEntryFields()$get(field)$getClass())
},

getFieldDef=function(field) {
    ":\n\nGets the definition of an entry field.
    \nfield: The name of the field.
    \nreturn: A object BiodbEntryField which defines the field.
    "
    return(.self$getBiodb()$getEntryFields()$get(field))
},

getFieldCardinality=function(field) {
    return(.self$getFieldDef(field)$getCardinality())
},

fieldHasBasicClass=function(field) {

    lifecycle::deprecate_warn('1.0.0', 'fieldHasBasicClass()',
        'BiodbEntryField::isVector()')

    return(.self$getBiodb()$getEntryFields()$get(field)$isVector())
},

.organizeFieldsByGroups=function(fields) {

    singles <- character()
    dfGrps <- list()
    ef <- .self$getBiodb()$getEntryFields()
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

.selectFields=function(fields, fields.type, own.id, only.atomic, only.card.one)
{

    logTrace('Fields %s', lst2str(fields))
    logTrace('Fields type: %s', fields.type)

    # Set fields to get
    logTrace('Fields is null: %s', is.null(fields))
    logTrace('Fields.type is null: %s', is.null(fields.type))
    if ( ! is.null(fields.type))
        fields <- .self$getFieldsByType(fields.type)
    else if (is.null(fields))
        fields <- names(.self$.fields)
    logTrace('Fields %s', lst2str(fields))

    # Filter out unwanted fields
    ef <- .self$getBiodb()$getEntryFields()
    if ( ! own.id) {
        ownIdField <- .self$getParent()$getEntryIdField()
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
    fields <- fields[fields %in% names(.self$.fields)]

    logTrace('Fields %s', lst2str(fields))
    return(fields)
},

.fieldsToDataframe=function(fields, duplicate.rows, flatten, limit,
                            duplicatedValues=TRUE) {

    # Transform values in data frames
    toDf <- function(f) {
        v <- .self$getFieldValue(f, flatten=flatten, limit=limit,
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
    outdf <- .self$.mergeDataframes(dataFrames, duplicate.rows=duplicate.rows)

    return(outdf)
},

.mergeDataframes=function(dataframes, duplicate.rows) {

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
