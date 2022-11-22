#' CSV File connector class.
#'
#' This is the abstract connector class for all CSV file databases.
#'
#' @seealso Super classes \code{\link{BiodbConn}},
#' and sub-classes \code{\link{CompCsvFileConn}},
#' \code{\link{MassCsvFileConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
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
#' @export
CsvFileConn <- R6::R6Class("CsvFileConn",
inherit=BiodbConn,

public=list(

#' @description
#' New instance initializer. Connector classes must not be instantiated
#' directly. Instead, you must use the createConn() method of the factory class.
#' @param ... All parameters are passed to the super class initializer.
#' @return Nothing.
initialize=function(...) {

    super$initialize(...)

    # Set fields
    private$db <- NULL
    private$db.orig.colnames <- NA_character_
    private$file.sep <- "\t"
    private$file.quote <- "\""
    private$fields <- character()
    private$field2cols <- list()
    private$autoSetFieldsHasBeenRun <- FALSE
    private$ignoreUnassignedColumns <- FALSE
    
    return(invisible(NULL))
},

#' @description
#' Gets the characters used to delimit quotes in the CSV database file.
#' @return The characters used to delimit quotes as a single
#'     character value.
getCsvQuote=function() {

    return(private$file.quote)
},

#' @description
#' Sets the characters used to delimit quotes in the CSV database file.
#' @param quote The characters used to delimit quotes as a single character
#' value.
#' @param You may specify several characters. Example \"\\\"'\".
#' @return Nothing.
setCsvQuote=function(quote) {

    chk::chk_string(quote)
    
    if ( ! is.null(private$db))
        error0("The CSV file has already been loaded. Modification of",
            " the quote parameter is not allowed.")
    
    private$file.quote <- quote

    return(invisible(NULL))
},

#' @description
#' Gets the current CSV separator used for the database file.
#' @return The CSV separator as a character value. 
getCsvSep=function() {

    return(private$file.sep)
},

#' @description
#' Sets the CSV separator to be used for the database file. If this method is
#' called after the loading of the database, it will throw an error.
#' @param sep The CSV separator as a character value.
#' @return Nothing.
setCsvSep=function(sep) {

    chk::chk_string(sep)
    
    if ( ! is.null(private$db))
        error0("The CSV file has already been loaded. Modification of",
            " the separator character parameter is not allowed.")
    
    private$file.sep <- sep

    return(invisible(NULL))
},

#' @description
#' Get the list of all biodb fields handled by this database.
#' @return A character vector of the biodb field names.
getFieldNames=function() {
    
    #private$initDb()
    fields <- names(private$fields)
    ef <- self$getBiodb()$getEntryFields()
    fct <- function(f) {
        return(ef$getRealName(f))
    }
    fields <- vapply(fields, fct, FUN.VALUE='')

    return(fields)
},

#' @description
#' Tests if a field is defined for this database instance.
#' @param field A valid Biodb entry field name.
#' @return TRUE of the field is defined, FALSE otherwise.
hasField=function(field) {

    if (is.null(field) || is.na(field))
        error0("No field specified.")

    return(private$doHasField(field))
},

#' @description
#' Adds a new field to the database. The field must not already exist.  The
#' same single value will be set to all entries for this field.  A new column
#' will be written in the memory data frame, containing the value given.
#' @param field A valid Biodb entry field name.
#' @param value The value to set for this field.
#' @return Nothing.
addField=function(field, value) {

    if (is.null(field) || is.na(field))
        error0("No field specified.")

    ef <- self$getBiodb()$getEntryFields()
    field <- ef$getRealName(field, fail=FALSE)

    # Load database file
    private$initDb()

    # Field already defined?
    if (field %in% self$getFieldNames())
        error0("Database field \"", field, "\" is already defined.")
    if (field %in% names(private$db))
        error0("Database column \"", field, "\" is already defined.")

    # Add new field
    logDebug0('Adding new field ', field, ' with value ',
        paste(value, collapse=', '), '.')
    private$db[[field]] <- value
    self$setField(field, field)

    return(invisible(NULL))
},

#' @description
#' Get the column name corresponding to a Biodb field.
#' @param field A valid Biodb entry field name. This field must be defined for
#' this database instance.
#' @return The column name from the CSV file.
getFieldColName=function(field) {

    if (is.null(field) || is.na(field))
        error0("No field specified.")

    ef <- self$getBiodb()$getEntryFields()
    field <- ef$getRealName(field)

    # Load database file
    private$initDb()

    # Check that this field is defined in the fields list
    if ( ! field %in% self$getFieldNames())
        error0("Database field \"", field, "\" is not defined.")

    return(private$fields[[field]])
},

#' @description
#' Sets a field by making a correspondence between a Biodb field and one or
#' more columns of the loaded data frame.
#' @param field A valid Biodb entry field name. This field must not be already
#' defined for this database instance.
#' @param colname A character vector containing one or more column names from
#' the CSV file.
#' @param ignore.if.missing Deprecated parameter.
#' @return Nothing.
setField=function(field, colname, ignore.if.missing=FALSE) {

    chk::chk_string(field)
    chk::chk_character(colname)
    chk::chk_not_any_na(colname)
    chk::chk_not_empty(colname)
    ef <- self$getBiodb()$getEntryFields()
    field <- ef$getRealName(field, fail=FALSE)

    # Load database file
    private$initDb(setFields=FALSE)

    # Check that this is a correct field name
    if ( ! ef$isDefined(field))
        error0("Database field \"", field, "\" is not valid.")

    # Fail if column names are not found in file
    if ( ! all(colname %in% names(private$db))) {
        undefined.cols <- colname[ ! colname %in% names(private$db)]
        error0("Column(s) ", paste(undefined.cols, collapse=", "), "
            is/are not defined in database file.")
    }

    # Fail if already defined
    if (field %in% names(private$fields))
        error0('Field "', field, '" is already set to "',
            private$fields[[field]], '".')

    logDebug0('Set field ', field, ' to column(s) ',
        paste(colname, collapse=', '), '.')

    # One column used, only
    if (length(colname) == 1) {

        # Check values
        if (self$getBiodb()$getEntryFields()$isDefined(field)) {
            entry.field <- self$getBiodb()$getEntryFields()$get(field)

            # Check values of enumerate type
            if (entry.field$isEnumerate()) {
                v <- private$db[[colname]]
                entry.field$checkValue(v)
                private$db[[colname]] <- entry.field$correctValue(v)
            }
        }


        # Set field
        private$fields[[field]] <- colname
        self$setPropValSlot('parsing.expr', field, colname, hook=FALSE)
    }

    # Use several column to join together
    else {
        fct <- function(i) { paste(private$db[i, colname], collapse='.') }
        # Create new column in memory data frame
        private$db[[field]] <- vapply(seq(nrow(private$db)), fct, FUN.VALUE='')
        private$fields[[field]] <- field
    }
    
    # Store column(s)/field association
    private$field2cols[[field]] <- colname

    return(invisible(NULL))
},

#' @description
#' Gets the association between biodb field names and CSV file column names.
#' @return A list with names being the biodb field names and values being a
#' character vector of column names from the CSV file.
getFieldsAndColumnsAssociation=function() {
    private$initDb()
    return(private$field2cols)
},

#' @description
#' Gets the list of unassociated column names from the CSV file.
#' @return A character vector containing column names.
getUnassociatedColumns=function() {
    private$initDb()
    cols <- colnames(private$db)
    used_cols <- unlist(private$field2cols)
    unused_cols <- cols[ ! cols %in% used_cols]
    return(unused_cols)
},

#' @description
#' Prints a description of this connector.
#' @return Nothing.
print=function() {
    # Overrides super class' method.
    
    private$initDb()
    super$print()
    
    # Display defined fields
    fields <- names(private$fields)
    if (length(fields) > 0)
        cat("The following fields have been defined: ",
            paste(fields, collapse=", "), ".\n", sep='')

    # Display unassociated columns
    cols <- self$getUnassociatedColumns()
    if (length(cols) > 0)
        cat("Unassociated columns: ", paste(cols, collapse=", "), ".\n", sep='')

    return(invisible(NULL))
},


#' @description
#' Sets the database directly from a data frame. You must not have set
#'     the database previously with the URL parameter.
#' @param db A data frame containing your database.
#' @return Nothing.
setDb=function(db) {

    # URL point to an existing file?
    url <- self$getPropValSlot('urls', 'base.url')
    if ( ! is.null(url) && ! is.na(url) && file.exists(url))
        error0('Cannot set this data frame as database. A URL that',
            ' points to an existing file has already been set for the',
            ' connector.')

    private$doSetDb(db)

    return(invisible(NULL))
},

#' @description
#' Tells the connector to ignore or not the columns found in the CSV file for
#' which no assignment were found.
#' @param ignore Set to TRUE to ignore the unassigned columns, and to FALSE
#' otherwise.
#' @return Nothing.
setIgnoreUnassignedColumns=function(ignore) {

    chk::chk_flag(ignore)
    private$ignoreUnassignedColumns <- ignore

    return(invisible(NULL))
}
),

private=list(
    file.sep=NULL,
    file.quote=NULL,
    db=NULL,
    db.orig.colnames=NULL,
    fields=NULL,
    field2cols=NULL,
    autoSetFieldsHasBeenRun=NULL,
    ignoreUnassignedColumns=NULL,
    parsingExprDefined=FALSE

,doHasField=function(field) {

    ef <- self$getBiodb()$getEntryFields()
    field <- ef$getRealName(field, fail=FALSE)

    # Load database file
    private$initDb()

    return(field %in% self$getFieldNames())
}
    
,doDefineParsingExpressions=function() {

    if ( ! private$parsingExprDefined) {

        private$initDb()

        # Define a parsing expression for each column inside the database
        for (field in self$getFieldNames())
            self$setPropValSlot('parsing.expr', field, private$fields[[field]],
                hook=FALSE)

        private$parsingExprDefined <- TRUE
    }

    return(invisible(NULL))
}

,doWrite=function() {

    logInfo0('Write all entries into "',
        self$getPropValSlot('urls', 'base.url'), '".')

    # Make sure all entries are loaded into cache.
    entry.ids <- self$getEntryIds()
    entries <- self$getBiodb()$getFactory()$getEntry(self$getId(), entry.ids)

    # Get all entries: the ones loaded from the database file and the ones
    # created in memory (and not saved).
    entries <- self$getAllVolatileCacheEntries()

    # Get data frame of all entries
    df <- self$getBiodb()$entriesToDataframe(entries, only.atomic=FALSE)

    # Write data frame
    write.table(df, file=self$getPropValSlot('urls', 'base.url'),
        row.names=FALSE, sep="\t", quote=FALSE)
},

initDb=function(setFields=TRUE) {

    if (is.null(private$db)) {

        # Check file
        file <- self$getPropValSlot('urls', 'base.url')
        if ( ! is.null(file) && ! is.na(file) && ! file.exists(file)
            && ! self$writingIsAllowed())
            error0("Cannot locate the file database \"", file, "\".")

        # No file to load, create empty database
        if (is.null(file) || is.na(file) || ! file.exists(file)) {
            logInfo("Creating empty database.")
            db <- data.frame(accession=character(), stringsAsFactors=FALSE)
        }

        # Load database
        else {
            logInfo('Loading file database "%s".', file)
            db <- read.table(file, sep=private$file.sep,
                quote=private$file.quote, header=TRUE, stringsAsFactors=FALSE,
                row.names=NULL, comment.char='', check.names=FALSE, fill=FALSE)
        }

        # Set database
        private$doSetDb(db)
    }
    
    # Auto set fields
    if (setFields && ! private$autoSetFieldsHasBeenRun)
        private$autoSetFields()
},

checkFields=function(fields, fail=TRUE) {

    if (length(fields) == 0 || (length(fields) == 1 && is.na(fields)))
        return

    # Check if fields are known
    fct <- function(f) self$getBiodb()$getEntryFields()$isDefined(f)
    unknown.fields <- fields[ ! vapply(fields, fct, FUN.VALUE=FALSE)]
    if (length(unknown.fields) > 0)
        error0("Field(s) ", paste(fields, collapse=", "),
            " is/are unknown.")

    # Init db
    private$initDb()

    # Check if fields are defined in file database
    undefined.fields <- fields[ ! fields %in% self$getFieldNames()]
    if (length(undefined.fields) > 0) {
        msg <- sprintf("Field(s) %s is/are undefined in file database.",
            paste(undefined.fields, collapse=", "))
        if (fail) error(msg) else logDebug(fail)
        return(FALSE)
    }

    return(TRUE)
},

selectByIds=function(db, ids) {

    private$checkFields('accession')
    db <- db[db[[private$fields[['accession']]]] %in% ids, , drop=FALSE]

    return(db)
},

select=function(db=NULL, ids=NULL, cols=NULL, drop=FALSE, uniq=FALSE,
    sort=FALSE, max.rows=0, ...) {
    
    chk::chk_number(max.rows)
    chk::chk_gte(max.rows, 0)

    # Get database
    if (is.null(db)) {
        private$initDb()
        db <- private$db
    }

    # Filtering
    if ( ! is.null(ids))
        db <- private$selectByIds(db, ids)
    db <- private$doSelect(db, ...)

    # Get subset of columns
    if ( ! is.null(cols)) {
        private$checkFields(cols)
        db <- db[, private$fields[cols], drop=FALSE]
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

selectByRange=function(db, field, minValue, maxValue) {
    
    # Get database
    if (is.null(db)) {
        private$initDb()
        db <- private$db
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
    private$checkFields(field)
    f <- private$fields[[field]]
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
            &
            (if (is.na(maxV)) rep(TRUE, length(values)) else  values <= maxV))
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

selectBySubstring=function(db, field, value) {
    
    # Get database
    if (is.null(db)) {
        private$initDb()
        db <- private$db
    }

    # Check field
    private$checkFields(field)
    f <- private$fields[[field]]
    values <- db[[f]]
    logDebug('%d values to filter on field %s.', length(values), field)

    # Filter
    db <- db[grep(value, values), , drop=FALSE]

    return(db)
},

checkSettingOfUrl=function(key, value) {

    # Setting of base URL
    if (key == 'base.url') {
        url <- self$getPropValSlot('urls', 'base.url')
        if ( ! is.null(private$db) && ! is.null(url) && ! is.na(url)
            && file.exists(url))
            error0('You cannot overwrite base URL. A URL has already',
                ' been set ("', url, '") that points to a valid file',
                ' that has already been loaded in memory.')
    }
},

doSetDb=function(db) {

    # Already set?
    if ( ! is.null(private$db))
        biodb::error('Database has already been set.')

    # Not a data frame
    if ( ! is.data.frame(db))
        biodb::error('The database object must be a data frame.')

    # Set data frame as database
    private$db <- db

    # Save column names
    private$db.orig.colnames <- colnames(private$db)
},

autoSetFields=function() {

    # Get fields definitions
    ef <- self$getBiodb()$getEntryFields()
    
    # Get columns already defined
    cols <- unname(private$fields)

    # Loop on all columns of database
    for (colname in names(private$db)) {
        
        # Is this column already set?
        if (colname %in% cols)
            next
        
        # Does this column name match a biodb field?
        else if (ef$isDefined(colname))
            self$setField(field=colname, colname=colname)
        
        # Column is not matchable
        else if ( ! private$ignoreUnassignedColumns)
            warn('Column "%s" does not match any biodb field.', colname)
    }
    
    # Mark as run
    private$autoSetFieldsHasBeenRun <- TRUE
},

doGetEntryIds=function(max.results=0) {

    ids <- NA_character_

    ids <- as.character(private$select(cols='accession', drop=TRUE, uniq=TRUE,
        sort=TRUE, max.rows=max.results))

    return(ids)
}

,doGetEntryContentFromDb=function(id) {

    # Initialize return values
    content <- rep(NA_character_, length(id))

    # Get data frame
    logDebug("Entry id: %s", paste(id, collapse=", "))
    df <- private$select(ids=id, uniq=TRUE, sort=TRUE)

    # For each id, take the sub data frame and convert it into string
    fct <- function(x) {
        if (is.na(x))
            NA_character_
        else {
            x.df <- private$select(ids=x)
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
    content <- vapply(id, fct, FUN.VALUE='')

    if (length(content) > 0)
        logDebug("Content of first entry: %s", content[[1]])

    return(content)
}

,doGetNbEntries=function(count=FALSE) {

    n <- NA_integer_

    ids <- self$getEntryIds()
    if ( ! is.null(ids))
        n <- length(ids)

    return(n)
}
))
