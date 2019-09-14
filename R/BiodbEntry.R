# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbEntry {{{1
################################################################################

# Declaration {{{2
################################################################################

#' The mother abstract class of all database entry classes.
#'
#' An entry is an element of a database, identifiable by its accession number.
#' Each contains a list of fields defined by a name and a value. The details of
#' all fields that can be set into an entry are defined inside the class
#' \code{BiodbEntryFields}. From this class are derived other abstract classes
#' for different types of entry contents: \code{BiodbTxtEntry},
#' \code{BiodbXmlEntry}, \code{BiodbCsvEntry}, \code{BiodbJsonEntry} and
#' \code{BiodbHtmlEntry}. Then concrete classes are derived for each database:
#' \code{ChebiEntry}, \code{ChemspiderEntru}, etc. For biodb users, there is no
#' need to know this hierarchy; the knowledge of this class and its methods is
#' sufficient.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{BiodbConn}},
#' \code{\link{BiodbEntryFields}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get an entry:
#' entry <- mybiodb$getFactory()$getEntry('chebi', '1')
#'
#' # Get all defined fields:
#' entry$getFieldNames()
#'
#' # Get a field value:
#' smiles <- entry$getFieldValue('smiles')
#'
#' # Test if a field is defined:
#' if (entry$hasField('charge'))
#'   print(paste('The entry has a charge of ', entry$getFieldValue('charge'),
#'   '.', sep=''))
#'
#' # Export an entry as a data frame:
#' df <- entry$getFieldsAsDataFrame()
#'
#' # Even if you may not do it, you can set a field's value yourselves:
#' entry$setFieldValue('mass', 1893.1883)
#'
#' # Or even add a new field:
#' entry$setFieldValue('chemspider.id', '388394')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @include BiodbChildObject.R
#' @export BiodbEntry
#' @exportClass BiodbEntry
BiodbEntry <- methods::setRefClass("BiodbEntry",
    contains="BiodbChildObject",
    fields=list(
        .fields='list',
        .new='logical'
    ),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)
    .self$.abstractClass('BiodbEntry')

    .self$.fields <- list()
    .self$.new <- FALSE
},

# Parent is connector {{{3
################################################################################

parentIsAConnector=function() {
    ":\n\nTests if the parent of this entry is a connector instance.
    \nReturned value: TRUE if this entry belongs to a connector, FALSE
    otherwise.
    "

    return(is(.self$getParent(), "BiodbConn"))
},

# Clone {{{3
################################################################################

clone=function(db.class=NULL) {
    ":\n\nClones this entry.
    \ndb.class: The database class (the Biodb database ID) of the clone. By
    setting this parameter, you can specify a different database for the clone,
    so you may clone a Massbank entry into a MassCsvFile entry if you wish. By
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

# Get ID {{{3
################################################################################

getId=function() {
    ":\n\nGets the entry ID.
    \nReturned value: the entry ID, which is the value if the `accession` field.
    "

    return(.self$getFieldValue('accession'))
},

# Is new {{{3
################################################################################

isNew=function() {
    ":\n\nTests if this entry is new.
    \nReturned value: TRUE if this entry was newly created, FALSE otherwise.
    "

    return(.self$.new)
},

# Get database class {{{3
################################################################################

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

# Set field value {{{3
################################################################################

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
      .self$error('Cannot set a non RC instance to field "', field,
                  '" in BiodEntry.')

    # Check value class
    if (field.def$isVector()) {
        if (length(value) == 0)
            .self$error('Cannot set an empty value into field "', field, '".')
        v <- as.vector(value, mode=field.def$getClass())
        if ( ! all(is.na(value)) && all(is.na(v)))
            .self$caution("Unable to convert value(s) \"",
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
    if (field.def$forbidsDuplicates() || (field.def$isVector()
                                          && field.def$hasCardOne()))
        value <- value[ ! duplicated(if (field.def$isCaseInsensitive())
                                     tolower(value) else value)]

    # Check cardinality
    if ( ! field.def$isDataFrame() && field.def$hasCardOne()) {
        if (length(value) > 1)
            .self$error('Cannot set more that one value (',
                        paste(value, collapse=', '),
                        ') into single value field "', field, '" for entry ',
                        .self$getName(), '.')
        if (length(value) == 0)
            .self$error('Cannot set an empty vector into single value field "',
                        field, '" for entry ', .self$getName(), '.')
    }

    # Set value
    .self$.fields[[field.def$getName()]] <- value
},

# Append field value {{{3
################################################################################

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

# Get field names {{{3
################################################################################

getFieldNames=function() {
    ":\n\nGets a list of all fields defined for this entry.
    \nReturned value: A character vector containing all field names defined in
    this entry.
    "

    return(sort(names(.self$.fields)))
},

# Has field {{{3
################################################################################

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

# Remove field {{{3
################################################################################

removeField=function(field) {
    ":\n\nRemoves the specified field from this entry.
    \nfield: The name of a field.
    \nReturned value: None.
    "

    if (.self$hasField(field))
        .self$.fields <- .self$.fields[names(.self$.fields) != tolower(field)]
},

# Get field value {{{3
################################################################################

getFieldValue=function(field, compute=TRUE, flatten=FALSE, last=FALSE) {
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
    \nReturned value: The value of the field.
    "

    val <- NULL
    field <- tolower(field)
    cfg <- .self$getBiodb()$getConfig()

    # Get field definition
    field.def <- .self$getBiodb()$getEntryFields()$get(field)
    field <- field.def$getName()

    # Compute field value
    if (compute && ! .self$hasField(field))
        .self$computeFields(field)

    # Get value
    if (.self$hasField(field))
        val <- .self$.fields[[field]]
    else
        # Return NULL or NA
        val <- if (field.def$isVector())
            as.vector(NA, mode=field.def$getClass()) else NULL

    # Get last value only
    if (last && field.def$hasCardMany() && length(val) > 1)
        val <- val[[length(val)]]

    # Flatten: convert atomic values with cardinality > 1 into a string
    if (flatten && ! is.null(val)) {
        if (field.def$isVector() && field.def$hasCardMany()
            && length(val) > 1) {
            if (all(is.na(val)))
                val <-  as.vector(NA, mode=field.def$getClass())
            else
                val <- paste(val, collapse=cfg$get('multival.field.sep'))
        }
    }

    return(val)
},

# Get fields as data frame {{{3
################################################################################

getFieldsAsDataFrame=function(only.atomic=TRUE, compute=TRUE, fields=NULL,
                              flatten=TRUE, only.card.one=FALSE) {
    ":\n\nConverts this entry into a data frame.
    \nonly.atomic: If set to TRUE, only export field's values that are atomic
    (i.e.: of type vector and length one).
    \ncompute: If set to TRUE and a field is not defined, try to compute it
    using internal defined computing rules. If set to FALSE, let the field
    undefined.
    \nfields: Set to character vector of field names in order to restrict
    execution to this set of fields.
    \nflatten: If set to TRUE and a field's value is a vector of more than one
    element, then export the field's value as a single string composed of the
    field's value concatenated and separated by the character defined in the
    'multival.field.sep' config key. If set to FALSE or the field contains only
    one value, changes nothing.
    \nonly.card.one: If set to TRUE, only fields with a cardinality of one will
    be extracted.
    \nReturned value: A data frame containg the values of the fields.
    "

    df <- data.frame(stringsAsFactors=FALSE)
    if ( ! is.null(fields))
        fields <- tolower(fields)

    # Compute fields
    if (compute)
        .self$computeFields(fields)

    # Set fields to get
    fields <- if (is.null(fields)) names(.self$.fields)
        else fields[fields %in% names(.self$.fields)]

    # Loop on fields
    for (f in fields) {

        field.def <- .self$getBiodb()$getEntryFields()$get(f)

        # Ignore non atomic values
        if (only.atomic && ! field.def$isVector())
            next

        # Ignore field with cardinality > one
        if (only.card.one && ! field.def$hasCardOne())
            next

        v <- .self$getFieldValue(f, flatten=flatten)

        # Transform vector into data frame
        if (is.vector(v)) {
            v <- as.data.frame(v, stringsAsFactors=FALSE)
            colnames(v) <- f
        }

        # Merge value into data frame
        if (is.data.frame(v) && nrow(v) > 0)
            df <- if (nrow(df) == 0) v else merge(df, v)
    }

    return(df)
},

# Get fields as json {{{3
################################################################################

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

    return(jsonlite::toJSON(.self$.fields, pretty=TRUE, digits=NA_integer_))
},

# Parse content {{{3
################################################################################

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
        .self$error('Impossible to parse content for this entry, because its',
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
            .self$error('Value of accession field ("',
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

# Compute fields {{{3
################################################################################

computeFields=function(fields=NULL) {
    ":\n\nComputes fields. Look at all missing fields, and try to compute them
    using references to other databases, if a rule exists.
    \nfields: A list of fields to review for computing. By default all fields
    will be reviewed.
    \nReturned value: None.
    "

    success <- FALSE
    ef <- .self$getBiodb()$getEntryFields()
    if ( ! is.null(fields))
        fields <- tolower(fields)

    if (.self$getBiodb()$getConfig()$isEnabled('compute.fields')) {

        # Set of fields to compute
        if (is.null(fields))
            fields <- ef$getFieldNames()

        # Loop on all fields
        for(f in fields) {

            # Skip this field if we already have a value for it
            if (.self$hasField(f))
                next

            # Loop on all databases where we can look for a value
            for (db in ef$get(f)$getComputableFrom()) {

                # Database is itself
                if ( ! methods::is(.self$getParent(), 'BiodbConn')
                    || db == .self$getParent()$getId())
                    next

                # Have we a reference for this database?
                db.id.field <- paste(db, 'id', sep='.')
                if ( ! .self$hasField(db.id.field))
                    next
                db.id <- .self$getFieldValue(db.id.field, compute=FALSE)
                if ( ! is.na(db.id)) {

                    # Get value for this field in the database
                    .self$debug("Compute value for field \"", f, "\".") 
                    db.entry <- .self$getBiodb()$getFactory()$getEntry(db,
                                                                       id=db.id)

                    # Set found value
                    if ( ! is.null(db.entry)) {
                        v <- db.entry$getFieldValue(f, compute=FALSE)
                        .self$setFieldValue(f, v)
                        success <- TRUE
                        break
                    }
                }
            }
        }
    }

    return(success)
},

# Show {{{3
################################################################################

show=function() {
    ":\n\nDisplays short information about this instance.
    \nReturned value: None.
    "

    db <- .self$getParent()$getName()
    id <- .self$getFieldValue('accession', compute=FALSE)
    id <- if (is.na(id)) 'ID unknown' else id
    cat("Biodb ", db, " entry instance ", id, ".\n", sep='')
},

# Get name {{{3
################################################################################

getName=function() {
    ":\n\nGets a short text describing this entry instance.
    \nReturned value: A character value concatenating the connector name with
    the entry accession.
    "

    name <- paste(.self$getParent()$getName(), .self$getFieldValue('accession'))

    return(name)
},

# Makes reference to entry  {{{3
################################################################################

makesRefToEntry=function(db, oid, recurse=FALSE) {
    ":\n\nTests if this entry makes reference to another entry.
    \ndb: Another database connector.
    \noid: A entry ID from database db.
    \nrecurse: If set to TRUE, the algorithm will follow all references to
    entries from other databases, to see if it can establish an indirect link
    to `oid`.
    \nReturned value: TRUE if this entry makes reference to the entry oid from database
    db, FALSE otherwise.
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

# Private methods {{{2
################################################################################

# Makes reference to entry  {{{3
################################################################################

.makesRefToEntryRecurse=function(db, oid) {
    return(FALSE)
},

# Set as new {{{3
################################################################################

.setAsNew=function(new) {
    .self$.new <- new
},

# Is content correct {{{3
################################################################################

.isContentCorrect=function(content) {

    correct <- ! is.null(content) && ((is.list(content) && length(content) > 0)
                                      || (is.character(content)
                                          && ! is.na(content) && content != ''))
    # NOTE `nchar(content)` may give "invalid multibyte string, element 1" on
    # some strings.

    return(correct)
},

# Do parse content {{{3
################################################################################

.doParseContent=function(content) {
    .self$.abstractMethod()
},

# Is parsed content correct {{{3
################################################################################

.isParsedContentCorrect=function(parsed.content) {
    return( ! is.null(parsed.content)
           && ( ! is.vector(parsed.content) || length(parsed.content) > 1
               || ! is.na(parsed.content)))
},

# Parse fields step 1 {{{3
################################################################################

.parseFieldsStep1=function(parsed.content) {
    .self$.abstractMethod()
},

# Parse fields step 2 {{{3
################################################################################

.parseFieldsStep2=function(parsed.content) {
},

# Check database ID field {{{3
################################################################################

.checkDbIdField=function() {
},

# Deprecated methods {{{2
################################################################################

# Get Field {{{3
################################################################################

getField=function(field) {
    .self$.deprecatedMethod("getFieldValue()")
    return(.self$getFieldValue(field))
},

# Set Field {{{3
################################################################################

setField=function(field, value) {
    .self$.deprecatedMethod("setFieldValue()")
    .self$setFieldValue(field, value)
},

# Get field class {{{3
################################################################################

getFieldClass=function(field) {

    .self$.deprecatedMethod('Biodb::getEntryFields()$get(field)$getClass()')

    return(.self$getBiodb()$getEntryFields()$get(field)$getClass())
},

# Get field cardinality {{{3
################################################################################

getFieldCardinality=function(field) {

    msg <- 'BiodbEntryField::hasCardOne() or BiodbEntryField::hasCardMany()'
    .self$.deprecatedMethod(msg)

    return(.self$getBiodb()$getEntryFields()$get(field)$getCardinality())
},

# Field has basic class {{{3
################################################################################

fieldHasBasicClass=function(field) {

    .self$.deprecatedMethod('BiodbEntryField::isVector()')

    return(.self$getBiodb()$getEntryFields()$get(field)$isVector())
},

# Compute field {{{3
################################################################################

.computeField=function(fields=NULL) {

    .self$.deprecatedMethod('BiodbEntry::computeFields()')

    return(.self$computeFields(fields=fields))
}

))
