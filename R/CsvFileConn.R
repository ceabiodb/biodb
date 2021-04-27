#' CSV File connector class.
#'
#' This is the abstract connector class for all CSV file databases.
#'
#' @seealso Super classes \code{\link{BiodbConn}}, \code{\link{BiodbWritable}},
#' \code{\link{BiodbEditable}}, and sub-classes \code{\link{CompCsvFileConn}},
#' \code{\link{MassCsvFileConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::BiodbMain()
#'
#' # Get a connector that inherits from CsvFileConn:
#' chebi_file <- system.file("extdata", "chebi_extract.tsv", package="biodb")
#' conn <- mybiodb$getFactory()$createConn('comp.csv.file', url=chebi_file)
#'
#' # Get an entry
#' e <- conn$getEntry('1018')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbConn.R
#' @include BiodbEditable.R
#' @include BiodbWritable.R
#' @export CsvFileConn
#' @exportClass CsvFileConn
CsvFileConn <- methods::setRefClass("CsvFileConn",
    contains=c("BiodbConn", 'BiodbWritable', 'BiodbEditable'),
    fields=list(
        .file.sep="character",
        .file.quote="character",
        .db="ANY",
        .db.orig.colnames="character",
        .fields="character",
        .field2cols="list",
        .autoSetFieldsHasBeenRun="logical",
        .ignoreUnassignedColumns="logical"
                ),

methods=list(

initialize=function(...) {

    callSuper(...)

    # Set fields
    .self$.db <- NULL
    .self$.db.orig.colnames <- NA_character_
    .self$.file.sep <- "\t"
    .self$.file.quote <- "\""
    .self$.fields <- character()
    .self$.field2cols <- list()
    .self$.autoSetFieldsHasBeenRun <- FALSE
    .self$.ignoreUnassignedColumns <- FALSE
},

getCsvQuote=function() {
    ":\n\nGets the characters used to delimit quotes in the CSV database file.
    \nReturned value: The characters used to delimit quotes as a single
    character value.
    "

    return(.self$.file.quote)
},

setCsvQuote=function(quote) {
    ":\n\nSets the characters used to delimit quotes in the CSV database file.
    \nquote: The characters used to delimit quotes as a single character value.
    You may specify several characters. Example: \"\\\"'\".
    \nReturned value: None.
    "

    chk::chk_string(quote)
    
    if ( ! is.null(.self$.db))
        error0("The CSV file has already been loaded. Modification of",
              " the quote parameter is not allowed.")
    
    .self$.file.quote <- quote
},

getCsvSep=function() {
    ":\n\nGets the current CSV separator used for the database file.
    \nReturned value: The CSV separator as a character value. 
    "

    return(.self$.file.sep)
},

setCsvSep=function(sep) {
    ":\n\nSets the CSV separator to be used for the database file. If this
    method is called after the loading of the database, it will throw an error.
    \nsep: The CSV separator as a character value.
    \nReturned value: None.
    "

    chk::chk_string(sep)
    
    if ( ! is.null(.self$.db))
        error0("The CSV file has already been loaded. Modification of",
              " the separator character parameter is not allowed.")
    
    .self$.file.sep <- sep
},

getFieldNames=function() {
    ":\n\nGet the list of all biodb fields handled by this database.
    \nReturned value: A character vector of the biodb field names.
    "
    
    fields <- names(.self$.fields)
    ef <- .self$getBiodb()$getEntryFields()
    fct <- function(f) {
        return(ef$getRealName(f))
    }
    fields <- vapply(fields, fct, FUN.VALUE='')

    return(fields)
},

hasField=function(field) {
    ":\n\nTests if a field is defined for this database instance.
    \nfield: A valid Biodb entry field name.
    \nReturned value: TRUE of the field is defined, FALSE otherwise.
    "

    if (is.null(field) || is.na(field))
        error0("No field specified.")

    ef <- .self$getBiodb()$getEntryFields()
    field <- ef$getRealName(field, fail=FALSE)

    # Load database file
    .self$.initDb()

    return(field %in% .self$getFieldNames())
},

isSearchableByField=function(field) {
    # Overrides super class' method.

    v <- callSuper(field)
    v <- v && .self$hasField(field)
    
    return(v)
},

addField=function(field, value) {
    ":\n\nAdds a new field to the database. The field must not already exist.
    The same single value will be set to all entries for this field.
    A new column will be written in the memory data frame, containing the value
    given.
    \nfield: A valid Biodb entry field name.
    \nvalue: The value to set for this field.
    \nReturned value: None.
    "

    if (is.null(field) || is.na(field))
        error0("No field specified.")

    ef <- .self$getBiodb()$getEntryFields()
    field <- ef$getRealName(field, fail=FALSE)

    # Load database file
    .self$.initDb()

    # Field already defined?
    if (field %in% .self$getFieldNames())
        error0("Database field \"", field, "\" is already defined.")
    if (field %in% names(.self$.db))
        error0("Database column \"", field, "\" is already defined.")

    # Add new field
    logDebug0('Adding new field ', field, ' with value ',
             paste(value, collapse=', '), '.')
    .self$.db[[field]] <- value
    .self$setField(field, field)
},

getFieldColName=function(field) {
    ":\n\nGet the column name corresponding to a Biodb field.
    \nfield: A valid Biodb entry field name. This field must be defined for this
    database instance.
    \nReturned value: The column name from the CSV file.
    "

    if (is.null(field) || is.na(field))
        error0("No field specified.")

    ef <- .self$getBiodb()$getEntryFields()
    field <- ef$getRealName(field)

    # Load database file
    .self$.initDb()

    # Check that this field is defined in the fields list
    if ( ! field %in% .self$getFieldNames())
        error0("Database field \"", field, "\" is not defined.")

    return(.self$.fields[[field]])
},

setField=function(field, colname, ignore.if.missing=FALSE) {
    ":\n\nSets a field by making a correspondence between a Biodb field and one
    or more columns of the loaded data frame.
    \nfield: A valid Biodb entry field name. This field must not be already
    defined for this database instance.
    \ncolname: A character vector containing one or more column names from the
    CSV file.
    \nignore.if.missing: Deprecated parameter.
    \nReturned value: None.
    "

    chk::chk_string(field)
    chk::chk_character(colname)
    chk::chk_not_any_na(colname)
    chk::chk_not_empty(colname)
    ef <- .self$getBiodb()$getEntryFields()
    field <- ef$getRealName(field, fail=FALSE)

    # Load database file
    .self$.initDb(setFields=FALSE)

    # Check that this is a correct field name
    if ( ! ef$isDefined(field))
        error0("Database field \"", field, "\" is not valid.")

    # Fail if column names are not found in file
    if ( ! all(colname %in% names(.self$.db))) {
        undefined.cols <- colname[ ! colname %in% names(.self$.db)]
        error0("Column(s) ", paste(undefined.cols, collapse=", "), "
              is/are not defined in database file.")
    }

    # Fail if already defined
    if (field %in% names(.self$.fields))
        error0('Field "', field, '" is already set to "',
              .self$.fields[[field]], '".')

    logDebug0('Set field ', field, ' to column(s) ',
             paste(colname, collapse=', '), '.')

    # One column used, only
    if (length(colname) == 1) {

        # Check values
        if (.self$getBiodb()$getEntryFields()$isDefined(field)) {
            entry.field <- .self$getBiodb()$getEntryFields()$get(field)

            # Check values of enumerate type
            if (entry.field$isEnumerate()) {
                v <- .self$.db[[colname]]
                entry.field$checkValue(v)
                .self$.db[[colname]] <- entry.field$correctValue(v)
            }
        }


        # Set field
        .self$.fields[[field]] <- colname
        .self$setPropValSlot('parsing.expr', field, colname)
    }

    # Use several column to join together
    else {
        fct <- function(i) { paste(.self$.db[i, colname], collapse='.') }
        # Create new column in memory data frame
        .self$.db[[field]] <- vapply(seq(nrow(.self$.db)), fct, FUN.VALUE='')
        .self$.fields[[field]] <- field
    }
    
    # Store column(s)/field association
    .self$.field2cols[[field]] <- colname
},

getFieldsAndColumnsAssociation=function() {
    .self$.initDb()
    return(.self$.field2cols)
},

getUnassociatedColumns=function() {
    .self$.initDb()
    cols <- colnames(.self$.db)
    used_cols <- unlist(.self$.field2cols)
    unused_cols <- cols[ ! cols %in% used_cols]
    return(unused_cols)
},

show=function() {
    # Overrides super class' method.
    
    callSuper()
    
    # Display defined fields
    fields <- names(.self$.fields)
    if (length(fields) > 0)
        cat("The following fields have been defined: ",
            paste(fields, collapse=", "), ".\n", sep='')

    # Display unassociated columns
    cols <- .self$getUnassociatedColumns()
    if (length(cols) > 0)
        cat("Unassociated columns: ", paste(cols, collapse=", "), ".\n", sep='')
},

getNbEntries=function(count=FALSE) {
    # Overrides super class' method.

    n <- NA_integer_

    ids <- .self$getEntryIds()
    if ( ! is.null(ids))
        n <- length(ids)

    return(n)
},

getEntryContentFromDb=function(entry.id) {
    # Overrides super class' method.

    # Initialize return values
    content <- rep(NA_character_, length(entry.id))

    # Get data frame
    logDebug("Entry entry.id: %s", paste(entry.id, collapse=", "))
    df <- .self$.select(ids=entry.id, uniq=TRUE, sort=TRUE)

    # For each id, take the sub data frame and convert it into string
    fct <- function(x) {
        if (is.na(x))
            NA_character_
        else {
            x.df <- .self$.select(ids=x)
            if (nrow(x.df) == 0)
                NA_character_
            else {
                str.conn <- textConnection("str", "w", local=TRUE)
                write.table(x.df, file=str.conn, row.names=FALSE, quote=FALSE,
                            sep="\t")
                close(str.conn)
                paste(str, collapse="\n")
            }
        }
    }
    content <- vapply(entry.id, fct, FUN.VALUE='')

    if (length(content) > 0)
        logDebug("Content of first entry: %s", content[[1]])

    return(content)
},

setDb=function(db) {
    ":\n\nSets the database directly from a data frame. You must not have set
    the database previously with the URL parameter.
    \ndb: A data frame containing your database.
    \nReturned value: None.
    "

    # URL point to an existing file?
    url <- .self$getPropValSlot('urls', 'base.url')
    if ( ! is.null(url) && ! is.na(url) && file.exists(url))
        error0('Cannot set this data frame as database. A URL that',
              ' points to an existing file has already been set for the',
              ' connector.')

    .self$.doSetDb(db)
},

defineParsingExpressions=function() {
    # Overrides super class' method.

    entry.fields <- .self$getBiodb()$getEntryFields()

    # Define a parsing expression for each column inside the database
    for (field in .self$getFieldNames())
        .self$setPropValSlot('parsing.expr', field, .self$.fields[[field]])
},

.doWrite=function() {

    logInfo0('Write all entries into "',
            .self$getPropValSlot('urls', 'base.url'), '".')

    # Make sure all entries are loaded into cache.
    entry.ids <- .self$getEntryIds()
    entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), entry.ids)

    # Get all entries: the ones loaded from the database file and the ones
    # created in memory (and not saved).
    entries <- .self$getAllCacheEntries()

    # Get data frame of all entries
    df <- .self$getBiodb()$entriesToDataframe(entries, only.atomic=FALSE)

    # Write data frame
    write.table(df, file=.self$getPropValSlot('urls', 'base.url'),
                row.names=FALSE, sep="\t", quote=FALSE)
},

.initDb=function(setFields=TRUE) {

    if (is.null(.self$.db)) {

        # Check file
        file <- .self$getPropValSlot('urls', 'base.url')
        if ( ! is.null(file) && ! is.na(file) && ! file.exists(file)
            && ! .self$writingIsAllowed())
            error0("Cannot locate the file database \"", file, "\".")

        # No file to load, create empty database
        if (is.null(file) || is.na(file) || ! file.exists(file)) {
            logInfo("Creating empty database.")
            db <- data.frame(accession=character(), stringsAsFactors=FALSE)
        }

        # Load database
        else {
            logInfo('Loading file database "%s".', file)
            db <- read.table(.self$getPropValSlot('urls', 'base.url'),
                             sep=.self$.file.sep, quote=.self$.file.quote,
                             header=TRUE, stringsAsFactors=FALSE,
                             row.names=NULL, comment.char='', check.names=FALSE,
                             fill=FALSE)
        }

        # Set database
        .self$.doSetDb(db)
    }
    
    # Auto set fields
    if (setFields && ! .self$.autoSetFieldsHasBeenRun)
        .self$.autoSetFields()
},

.checkFields=function(fields, fail=TRUE) {

    if (length(fields) == 0 || (length(fields) == 1 && is.na(fields)))
        return

    # Check if fields are known
    fct <- function(f) .self$getBiodb()$getEntryFields()$isDefined(f)
    unknown.fields <- fields[ ! vapply(fields, fct, FUN.VALUE=FALSE)]
    if (length(unknown.fields) > 0)
        error0("Field(s) ", paste(fields, collapse=", "),
              " is/are unknown.")

    # Init db
    .self$.initDb()

    # Check if fields are defined in file database
    undefined.fields <- fields[ ! fields %in% .self$getFieldNames()]
    if (length(undefined.fields) > 0) {
        msg <- sprintf("Field(s) %s is/are undefined in file database.",
                       paste(undefined.fields, collapse=", "))
        if (fail) error(msg) else logDebug(fail)
        return(FALSE)
    }

    return(TRUE)
},

.selectByIds=function(db, ids) {

    .self$.checkFields('accession')
    db <- db[db[[.self$.fields[['accession']]]] %in% ids, , drop=FALSE]

    return(db)
},

.select=function(db=NULL, ids=NULL, cols=NULL, drop=FALSE, uniq=FALSE,
                 sort=FALSE, max.rows=0, ...) {
    
    chk::chk_number(max.rows)
    chk::chk_gte(max.rows, 0)

    # Get database
    if (is.null(db)) {
        .self$.initDb()
        db <- .self$.db
    }

    # Filtering
    if ( ! is.null(ids))
        db <- .self$.selectByIds(db, ids)
    db <- .self$.doSelect(db, ...)

    # Get subset of columns
    if ( ! is.null(cols)) {
        .self$.checkFields(cols)
        db <- db[, .self$.fields[cols], drop=FALSE]
    }

    # Remove duplicates
    if (uniq)
        db <- db[ ! duplicated(db), , drop=FALSE]

    # Sort on first column
    if (sort && ncol(db) >= 1)
        db <- db[order(db[[1]]), , drop=FALSE]

    # Cut
    if (max.rows > 0 && nrow(db) > max.rows)
        db <- db[seq_len(max.rows), , drop=FALSE]

    # Drop
    if (drop && ncol(db) == 1)
        db <- db[[1]]

    return(db)
},

.selectByRange=function(db, field, minValue, maxValue) {
    
    # Get database
    if (is.null(db)) {
        .self$.initDb()
        db <- .self$.db
    }

    # Check range
    if (is.null(minValue) || is.null(maxValue))
        error0('You must set both min and max values.')
    if (length(minValue) != length(maxValue))
        error0("'minValue' and 'maxValue' must have equal lengths.",
              " 'minValue' has ", length(minValue), " element(s),",
              " and 'maxValue' has ", length(maxValue), "element(s).")
    logDebug0('Filtering on field "', field, '", with range: ',
             paste0('[', minValue, ', ', maxValue, ']', collapse=', '), '.')
    
    # Check field
    .self$.checkFields(field)
    f <- .self$.fields[[field]]
    values <- db[[f]]
    logDebug('%d values to filter on field %s.', length(values), field)

    # For all couples in vectors minValue and maxValue, verify which values
    # are in the range. For each couple of minValue/maxValue we get a vector of
    # booleans the same length as `values` vector.
    fct <- function(minV, maxV) {
        if (is.na(minV) && is.na(maxV))
            rep(FALSE, length(values))
        else
            ((if (is.na(minV)) rep(TRUE, length(values)) else values >= minV)
             & (if (is.na(maxV)) rep(TRUE, length(values)) else  values <= maxV))
    }
    s <- mapply(fct, minValue, maxValue)

    # Now we select the values that are in at least one of the ranges.
    if (is.matrix(s))
        s <- apply(s, 1, function(x) Reduce("|", x))
    else if (is.list(s))
        s <- unlist(s)

    # Filter
    db <- db[s, , drop=FALSE]

    return(db)
},

.selectBySubstring=function(db, field, value) {
    
    # Get database
    if (is.null(db)) {
        .self$.initDb()
        db <- .self$.db
    }

    # Check field
    .self$.checkFields(field)
    f <- .self$.fields[[field]]
    values <- db[[f]]
    logDebug('%d values to filter on field %s.', length(values), field)

    # Filter
    db <- db[grep(value, values), , drop=FALSE]

    return(db)
},

.checkSettingOfUrl=function(key, value) {

    # Setting of base URL
    if (key == 'base.url') {
        url <- .self$getPropValSlot('urls', 'base.url')
        if ( ! is.null(.self$.db) && ! is.null(url) && ! is.na(url)
            && file.exists(url))
            error0('You cannot overwrite base URL. A URL has already',
                  ' been set ("', url, '") that points to a valid file',
                  ' that has already been loaded in memory.')
    }
},

.doSetDb=function(db) {

    # Already set?
    if ( ! is.null(.self$.db))
        .self$message('error', 'Database has already been set.')

    # Not a data frame
    if ( ! is.data.frame(db))
        .self$message('error', 'The database object must be a data frame.')

    # Set data frame as database
    .self$.db <- db

    # Save column names
    .self$.db.orig.colnames <- colnames(.self$.db)
},

ignoreUnassignedColumns=function(ignore=TRUE) {
    .self$.ignoreUnassignedColumns <- ignore
},

.autoSetFields=function() {

    # Get fields definitions
    ef <- .self$getBiodb()$getEntryFields()
    
    # Get columns already defined
    cols <- unname(.self$.fields)

    # Loop on all columns of database
    for (colname in names(.self$.db)) {
        
        # Is this column already set?
        if (colname %in% cols)
            next
        
        # Does this column name match a biodb field?
        else if (ef$isDefined(colname))
            .self$setField(field=colname, colname=colname)
        
        # Column is not matchable
        else if ( ! .self$.ignoreUnassignedColumns)
            warn('Column "%s" does not match any biodb field.', colname)
    }
    
    # Mark as run
    .self$.autoSetFieldsHasBeenRun <- TRUE
},

.doGetEntryIds=function(max.results=0) {
    # Overrides super class' method.

    ids <- NA_character_

    ids <- as.character(.self$.select(cols='accession', drop=TRUE, uniq=TRUE,
                                      sort=TRUE, max.rows=max.results))

    return(ids)
}
))
