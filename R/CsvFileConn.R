#' CSV File connector class.
#'
#' This is the abstract connector class for all CSV file databases.
#'
#' @seealso Super class \code{\link{BiodbConn}}, and sub-classes
#' \code{\link{CompoundCsvFileConn}} and \code{\link{MassCsvFileConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get a connector that inherits from CsvFileConn:
#' chebi_file <- system.file("extdata", "chebi_extract.tsv", package="biodb")
#' conn <- mybiodb$getFactory()$createConn('comp.csv.file', url=chebi_file)
#'
#' # Get an entry
#' e <- conn$getEntry('')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbConn.R
#' @export CsvFileConn
#' @exportClass CsvFileConn
CsvFileConn <- methods::setRefClass("CsvFileConn",
    contains="BiodbConn",
    fields=list(
        .file.sep="character",
        .file.quote="character",
        .db="ANY",
        .db.orig.colnames="character",
        .fields="character",
        .parsing.expr='list'
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
    .self$.parsing.expr <- list()
},

hasField=function(field) {
    ":\n\nTests if a field is defined for this database instance.
    \nfield: A valid Biodb entry field name.
    \nReturned value: TRUE of the field is defined, FALSE otherwise.
    "

    field <- tolower(field)

    if (is.null(field) || is.na(field))
        .self$error("No field specified.")

    # Load database file
    .self$.initDb()

    return(field %in% names(.self$.fields))
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

    .self$.checkParsingHasBegan()

    field <- tolower(field)

    if (is.null(field) || is.na(field))
        .self$error("No field specified.")

    # Load database file
    .self$.initDb()

    # Field already defined?
    if (field %in% names(.self$.fields))
        .self$error("Database field \"", field, "\" is already defined.")
    if (field %in% names(.self$.db))
        .self$error("Database column \"", field, "\" is already defined.")

    # Add new field
    .self$debug('Adding new field ', field, ' with value ',
                paste(value, collapse=', '), '.')
    .self$.db[[field]] <- value
    .self$.fields[[field]] <- field
},

getFieldColName=function(field) {
    ":\n\nGet the column name corresponding to a Biodb field.
    \nfield: A valid Biodb entry field name. This field must be defined for this
    database instance.
    \nReturned value: The column name from the CSV file.
    "

    field <- tolower(field)

    if (is.null(field) || is.na(field))
        .self$error("No field specified.")

    # Load database file
    .self$.initDb()

    # Check that this field is defined in the fields list
    if ( ! field %in% names(.self$.fields))
        .self$error("Database field \"", field, "\" is not defined.")

    return(.self$.fields[[field]])
},

setField=function(field, colname, ignore.if.missing=FALSE) {
    ":\n\nSets a field by making a correspondence between a Biodb field and one
    or more columns of the loaded data frame.
    \nfield: A valid Biodb entry field name. This field must not be already
    defined for this database instance.
    \ncolname: A character vector contain one or more column names from the CSV
    file.
    \nignore.if.missing: If set to TRUE, does not raise an error if one of the
    columns does not exists in the CSV file.
    \nReturned value: None.
    "

    .self$.checkParsingHasBegan()

    field <- tolower(field)

    .self$.assertNotNull(field)
    .self$.assertNotNa(field)
    .self$.assertNotNull(colname)
    .self$.assertNotNa(colname)

    # Load database file
    .self$.initDb()

    # Check that this is a correct field name
    if ( ! .self$getBiodb()$getEntryFields()$isDefined(field)) {
        if ( ! ignore.if.missing)
            .self$error("Database field \"", field, "\" is not valid.")
        return()
    }

    # Set real name (i.e.: official name) for field
    field <- .self$getBiodb()$getEntryFields()$getRealName(field)

    # Fail if column names are not found in file
    if ( ! all(colname %in% names(.self$.db))) {
        undefined.cols <- colname[ ! colname %in% names(.self$.db)]
        .self$message((if (ignore.if.missing) 'caution' else 'error'),
                      paste0("Column(s) ", paste(undefined.cols, collapse=", "),
                             " is/are not defined in database file."))
        return()
    }

    .self$debug('Set field ', field, ' to column(s) ',
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
    }

    # Use several column to join together
    else {
        fct <- function(i) { paste(.self$.db[i, colname], collapse='.') }
        .self$.db[[field]] <- vapply(seq(nrow(.self$.db)), fct, FUN.VALUE='')
        .self$.fields[[field]] <- field
    }
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
    .self$debug("Entry entry.id: ", paste(entry.id, collapse=", "))
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
        .self$message('debug', paste("Content of first entry:", content[[1]]))

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
        .self$error('Cannot set this data frame as database. A URL that',
                    ' points to an existing file has already been set for the',
                    ' connector.')

    .self$.doSetDb(db)
},

defineParsingExpressions=function() {
    # Overrides super class' method.

    entry.fields <- .self$getBiodb()$getEntryFields()

    # Loop on all fields defined in database
    for (field in names(.self$.fields))
        .self$setPropValSlot('parsing.expr', field, .self$.fields[[field]])

    # Loop on all entry fields
    for (field in entry.fields$getFieldNames())
        if ( ! field %in% names(.self$.fields)) {
            f <- entry.fields$get(field)
            if ( ! f$isVirtual())
                .self$setPropValSlot('parsing.expr', field, field)
        }
},

.doWrite=function() {

    .self$info('Write all entries into "',
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

.initDb=function() {

    if (is.null(.self$.db)) {

        # Check file
        file <- .self$getPropValSlot('urls', 'base.url')
        if ( ! is.null(file) && ! is.na(file) && ! file.exists(file))
            .self$info("Cannot locate the file database \"", file, "\".")

        # No file to load
        if (is.null(file) || is.na(file) || ! file.exists(file)) {
            .self$message('info', "Creating empty database.")
            db <- data.frame(accession=character(), stringsAsFactors=FALSE)
        }

        # Load database
        else {
            .self$info("Loading file database \"", file, "\".")
            db <- read.table(.self$getPropValSlot('urls', 'base.url'),
                             sep=.self$.file.sep, quote=.self$.file.quote,
                             header=TRUE, stringsAsFactors=FALSE,
                             row.names=NULL, comment.char='', check.names=FALSE,
                             fill=FALSE)
        }

        # Set database
        .self$.doSetDb(db)
    }
},

.checkFields=function(fields, fail=TRUE) {

    if (length(fields) == 0 || (length(fields) == 1 && is.na(fields)))
        return

    # Check if fields are known
    fct <- function(f) .self$getBiodb()$getEntryFields()$isDefined(f)
    unknown.fields <- fields[ ! vapply(fields, fct, FUN.VALUE=FALSE)]
    if (length(unknown.fields) > 0)
        .self$error("Field(s) ", paste(fields, collapse=", "),
                    " is/are unknown.")

    # Init db
    .self$.initDb()

    # Check if fields are defined in file database
    undefined.fields <- fields[ ! fields %in% names(.self$.fields)]
    if (length(undefined.fields) > 0) {
        .self$message((if (fail) 'error' else 'debug'),
                      paste0("Field(s) ",
                             paste(undefined.fields, collapse=", "),
                             " is/are undefined in file database."))
        return(FALSE)
    }

    return(TRUE)
},

.selectByIds=function(db, ids) {

    .self$.checkFields('accession')
    db <- db[db[[.self$.fields[['accession']]]] %in% ids, , drop=FALSE]

    return(db)
},

.select=function(ids=NULL, cols=NULL, drop=FALSE, uniq=FALSE, sort=FALSE,
                 max.rows=NA_integer_, ...) {

    # Init db
    .self$.initDb()

    # Get db
    db <- .self$.db

    # Filtering
    if ( ! is.null(ids))
        db <- .self$.selectByIds(db, ids)
    db <- .self$.doSelect(db, ...)

    # Get subset of columns
    if ( ! is.null(cols) && ! is.na(cols)) {
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
    if ( ! is.na(max.rows) && max.rows > 0 && nrow(db) > max.rows)
        db <- db[seq_len(max.rows), , drop=FALSE]

    # Drop
    if (drop && ncol(db) == 1)
        db <- db[[1]]

    return(db)
},

.checkSettingOfUrl=function(key, value) {

    # Setting of base URL
    if (key == 'base.url') {
        url <- .self$getPropValSlot('urls', 'base.url')
        if ( ! is.null(.self$.db) && ! is.null(url) && ! is.na(url)
            && file.exists(url))
            .self$error('You cannot overwrite base URL. A URL has already',
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

    # Set fields
    for (field in names(.self$.db))
        .self$setField(field, field, ignore.if.missing=TRUE)

    # Save column names
    .self$.db.orig.colnames <- colnames(.self$.db)
},

.checkParsingHasBegan=function() {

    # Parsing has began?
    if (length(.self$.parsing.expr) > 0)
        .self$error('Action impossible, parsing of entries has already began.')
},

.doGetEntryIds=function(max.results=NA_integer_) {

    ids <- NA_character_

    ids <- as.character(.self$.select(cols='accession', drop=TRUE, uniq=TRUE,
                                      sort=TRUE, max.rows=max.results))

    return(ids)
}
))
