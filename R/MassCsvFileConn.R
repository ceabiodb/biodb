# vi: fdm=marker ts=4 et cc=80 tw=80

# MassCsvFileConn {{{1
################################################################################

#' @include BiodbMassdbConn.R 
#' @include BiodbEditable.R 
#' @include BiodbWritable.R 
MassCsvFileConn <- methods::setRefClass("MassCsvFileConn",
    contains=c("BiodbMassdbConn", 'BiodbWritable', 'BiodbEditable'),
    fields=list(
        .file.sep="character",
        .file.quote="character",
        .field.multval.sep='character',
        .db="ANY",
        .db.orig.colnames="character",
        .fields="character",
        .precursors="character",
        .parsing.expr='list'),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)

    # Set fields
    .self$.db <- NULL
    .self$.db.orig.colnames <- NA_character_
    .self$.file.sep <- "\t"
    .self$.file.quote <- "\""
    .self$.fields <- character()
    .self$.field.multval.sep <- ';'
    .self$.parsing.expr <- list()

    # Precursors
    .self$.precursors <- c("[(M+H)]+", "[M+H]+", "[(M+Na)]+", "[M+Na]+",
                           "[(M+K)]+", "[M+K]+", "[(M-H)]-", "[M-H]-",
                           "[(M+Cl)]-", "[M+Cl]-")
},

# Get precursor formulae {{{3
################################################################################

getPrecursorFormulae=function() {
    "Returns the list of formulae used to recognize precursors."
    return (.self$.precursors)
},

# Is a precursor formula {{{3
################################################################################

isAPrecursorFormula=function(formula) {
    "Returns TRUE of the submitted formula is considered a precursor."
    return (formula %in% .self$.precursors)
},

# Set precursor formulae {{{3
################################################################################

setPrecursorFormulae=function(formulae) {
    "Replace current formulae by this new list of formulae."
    .self$.assertIs(formulae, 'character')
    .self$.precursors <- formulae[ ! duplicated(formulae)]
},

# Add precursor formulae {{{3
################################################################################

addPrecursorFormulae=function(formulae) {
    "Add new formulae to the list of formulae used to recognize precursors."

    .self$.checkParsingHasBegan()

    if ( ! all(formulae %in% .self$.precursors)) {
        formulae <- formulae[ ! formulae %in% .self$.precursors]
        .self$.precursors <- c(.self$.precursors, formulae)
    }
},

# Is valid field tag {{{3
################################################################################

isValidFieldTag=function(tag) {
    return (tag %in% names(.self$.fields))
},

# Has field {{{3
################################################################################

hasField=function(tag) {

    tag <- tolower(tag)

    if (is.null(tag) || is.na(tag))
        .self$error("No tag specified.")

    # Load database file
    .self$.initDb()

    return(tag %in% names(.self$.fields))
},

# Add field {{{3
################################################################################

addField=function(tag, value) {
    "Add a new field (column) to the database (data frame)."

    .self$.checkParsingHasBegan()

    tag <- tolower(tag)

    if (is.null(tag) || is.na(tag))
        .self$error("No tag specified.")

    # Load database file
    .self$.initDb()

    # Field already defined?
    if (tag %in% names(.self$.fields))
        .self$error("Database field \"", tag, "\" is already defined.")
    if (tag %in% names(.self$.db))
        .self$error("Database column \"", tag, "\" is already defined.")

    # Add new field
    .self$debug('Adding new field ', tag, ' with value ',
                paste(value, collapse=', '), '.')
    .self$.db[[tag]] <- value
    .self$.fields[[tag]] <- tag
},

# Get field {{{3
################################################################################

getField=function(tag) {

    tag <- tolower(tag)

    if (is.null(tag) || is.na(tag))
        .self$error("No tag specified.")

    # Load database file
    .self$.initDb()

    # Check that this field tag is defined in the fields list
    if ( ! tag %in% names(.self$.fields))
        .self$error("Database field tag \"", tag, "\" is not defined.")

    return(.self$.fields[[tag]])
},

# Set field {{{3
################################################################################

setField=function(tag, colname, ignore.if.missing=FALSE) {

    .self$.checkParsingHasBegan()

    tag <- tolower(tag)

    .self$.assertNotNull(tag)
    .self$.assertNotNa(tag)
    .self$.assertNotNull(colname)
    .self$.assertNotNa(colname)

    # Load database file
    .self$.initDb()

    # Check that this is a correct field name
    if ( ! .self$getBiodb()$getEntryFields()$isDefined(tag)) {
        if ( ! ignore.if.missing)
            .self$error("Database field \"", tag, "\" is not valid.")
        return()
    }

    # Set real name (i.e.: official name) for field
    tag <- .self$getBiodb()$getEntryFields()$getRealName(tag)

    # Fail if column names are not found in file
    if ( ! all(colname %in% names(.self$.db))) {
        undefined.cols <- colname[ ! colname %in% names(.self$.db)]
        .self$message((if (ignore.if.missing) 'caution' else 'error'),
                      paste0("Column(s) ", paste(undefined.cols, collapse=", "),
                             " is/are not defined in database file."))
        return()
    }

    .self$debug('Set field ', tag, ' to column(s) ',
                paste(colname, collapse=', '), '.')
                  
    # One column used, only
    if (length(colname) == 1) {

        # Check values
        if (.self$getBiodb()$getEntryFields()$isDefined(tag)) {
            entry.field <- .self$getBiodb()$getEntryFields()$get(tag)

            # Check values of enumerate type
            if (entry.field$isEnumerate()) {
                v <- .self$.db[[colname]]
                entry.field$checkValue(v)
                .self$.db[[colname]] <- entry.field$correctValue(v)
            }
        }


        # Set field
        .self$.fields[[tag]] <- colname
    }

    # Use several column to join together
    else {
        fct <- function(i) { paste(.self$.db[i, colname], collapse='.') }
        .self$.db[[tag]] <- vapply(seq(nrow(.self$.db)), fct, FUN.VALUE='')
        .self$.fields[[tag]] <- tag
    }
},

# Set field multiple value separator {{{3
################################################################################

setFieldMultValSep=function(sep) {

    .self$.checkParsingHasBegan()

    .self$.field.multval.sep <- sep
},


# Get nb entries {{{3
################################################################################

getNbEntries=function(count=FALSE) {

    n <- NA_integer_

    ids <- .self$getEntryIds()
    if ( ! is.null(ids))
        n <- length(ids)

    return(n)
},

# Get chromatographic columns {{{3
################################################################################

getChromCol=function(ids=NULL) {

    # Extract needed columns
    db <- .self$.select(cols='chrom.col.name', ids=ids)

    # Get column names
    cols <- db[[.self$.fields[['chrom.col.name']]]]

    # Remove NA values
    cols <- cols[ ! is.na(cols)]

    # Remove duplicates
    cols <- cols[ ! duplicated(cols)]

    # Make data frame
    if (is.null(cols))
        chrom.cols <- data.frame(a=character(0), b=character(0))
    else
        chrom.cols <- data.frame(cols, cols, stringsAsFactors=FALSE)
    names(chrom.cols) <- c('id', 'title')

    return(chrom.cols)
},

# Get nb peaks {{{3
################################################################################

# Inherited from BiodbMassdbConn.
getNbPeaks=function(mode=NULL, ids=NULL) {

    # Get peaks
    peaks <- .self$.select(cols='peak.mztheo', mode=mode, ids=ids, drop=TRUE)

    return(length(peaks))
},

# Get entry content from database {{{3
################################################################################

getEntryContentFromDb=function(entry.id) {

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

# Set database {{{3
################################################################################

setDb=function(db) {
    "Set the database directly from a data frame. You must not have set the
    database previously with the URL parameter."

    # URL point to an existing file?
    url <- .self$getPropValSlot('urls', 'base.url')
    if ( ! is.null(url) && ! is.na(url) && file.exists(url))
        .self$error('Cannot set this data frame as database. A URL that',
                    ' points to an existing file has already been set for the',
                    ' connector.')

    .self$.doSetDb(db)
},

# Define parsing expressions {{{3
################################################################################

defineParsingExpressions=function() {

    entry.fields <- .self$getBiodb()$getEntryFields()

    # Loop on all fields defined in database
    for (field in names(.self$.fields)) {
        f <- entry.fields$get(field)
        if (is.null(f) || is.na(f$getGroup()) || f$getGroup() != 'peak')
            .self$setPropValSlot('parsing.expr', field, .self$.fields[[field]])
    }

    # Loop on all entry fields
    for (field in entry.fields$getFieldNames())
        if ( ! field %in% names(.self$.fields)) {
        f <- entry.fields$get(field)
        if (is.na(f$getGroup()) || f$getGroup() != 'peak')
            .self$setPropValSlot('parsing.expr', field, field)
    }
},

# Private methods {{{2
################################################################################

# Do write {{{3
################################################################################

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

# Init db {{{3
################################################################################

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

# Check fields {{{3
################################################################################

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

# Select by mode {{{3
################################################################################

.selectByMode=function(db, mode) {

    # Check mode value
    msModeField <- .self$getBiodb()$getEntryFields()$get('ms.mode')
    msModeField$checkValue(mode)
    .self$.checkFields('ms.mode')

    # Filter on mode
    field <- .self$.fields[['ms.mode']]
    modesVal <- msModeField$getAllowedValues(mode)
    db <- db[db[[field]] %in% modesVal, , drop=FALSE]

    return(db)
},

# Select by IDs {{{3
################################################################################

.selectByIds=function(db, ids) {

    .self$.checkFields('accession')
    db <- db[db[[.self$.fields[['accession']]]] %in% ids, , drop=FALSE]

    return(db)
},

# Select by compound IDs {{{3
################################################################################

.selectByCompoundIds=function(db, compound.ids) {

    .self$.checkFields('compound.id')
    field <- .self$.fields[['compound.id']]
    db <- db[db[[field]] %in% compound.ids, , drop=FALSE]

    return(db)
},

# Select by M/Z values {{{3
################################################################################

.selectByMzValues=function(db, mz.min, mz.max) {

    if (is.null(mz.min) || is.null(mz.max))
        .self$error('You must set both mz.min and mz.max.')
    if (length(mz.min) != length(mz.max))
        .self$error("'mz.min' and 'mz.max' must have equal lengths.",
                    " 'mz.min' has ", length(mz.min), " element(s),",
                    " and 'mz.max' has ", length(mz.max), "element(s).")

    .self$debug('Filtering on M/Z ranges: ', paste0('[', mz.min, ', ', mz.max,
                                                    ']', collapse=', '), '.')
    .self$.checkFields('peak.mztheo')
    f <- .self$.fields[['peak.mztheo']]
    mz <- db[[f]]
    .self$message('debug', paste(length(mz), 'M/Z values to filter.'))

    # For all couples in vectors mz.min and mz.max, verify which M/Z values in
    # mz are in the range. For each couple of mz.min/mz.max we get a vector of
    # booleans the same length as mz.
    fct <- function(mzmin, mzmax) {
        if (is.na(mzmin) && is.na(mzmax))
            rep(FALSE, length(mz))
        else
            ((if (is.na(mzmin)) rep(TRUE, length(mz)) else mz >= mzmin)
             & (if (is.na(mzmax)) rep(TRUE, length(mz)) else  mz <= mzmax))
    }
    s <- mapply(fct, mz.min, mz.max)

    # Now we select the M/Z values that are in at least one of the M/Z ranges.
    if (is.matrix(s))
        s <- apply(s, 1, function(x) Reduce("|", x))
    else if (is.list(s))
        s <- unlist(s)

    db <- db[s, , drop=FALSE]

    return(db)
},

# Select by relative intensity {{{3
################################################################################

.selectByRelInt=function(db, min.rel.int) {

    if (.self$.checkFields('peak.relative.intensity', fail=FALSE)) {
        field <- .self$.fields[['peak.relative.intensity']]
        db <- db[db[[field]] >= min.rel.int, , drop=FALSE]
    }
    else
        db <- db[integer(), , drop=FALSE]

    return(db)
},

# Select by precursors {{{3
################################################################################

.selectByPrecursors=function(db) {

    if (.self$.checkFields('peak.attr', fail=FALSE)) {
        field <- .self$.fields[['peak.attr']]
        db <- db[db[[field]] %in% .self$.precursors, , drop=FALSE]
    }
    else
        db <- db[integer(), , drop=FALSE]

    return(db)
},

# Select by MS level {{{3
################################################################################

.selectByMsLevel=function(db, level) {

    if (.self$.checkFields('ms.level', fail=FALSE))
        db <- db[db[[.self$.fields[['ms.level']]]] == level, , drop=FALSE]
    else
        db <- db[integer(), , drop=FALSE]

    return(db)
},

# Select {{{3
################################################################################

# Select data from database
.select=function(ids=NULL, cols=NULL, mode=NULL, compound.ids=NULL, drop=FALSE,
                 uniq=FALSE, sort=FALSE, max.rows=NA_integer_, mz.min=NULL,
                 mz.max=NULL, min.rel.int=NA_real_, precursor=FALSE, level=0) {

    # Init db
    .self$.initDb()

    # Get db
    db <- .self$.db

    # Filtering
    if ( ! is.null(mode) && ! is.na(mode))
        db <- .self$.selectByMode(db, mode)
    if ( ! is.null(ids))
        db <- .self$.selectByIds(db, ids)
    if ( ! is.null(compound.ids))
        db <- .self$.selectByCompoundIds(db, compound.ids)
    if ( ! is.null(mz.min) || ! is.null(mz.max))
        db <- .self$.selectByMzValues(db, mz.min, mz.max)
    if ( ! is.na(min.rel.int))
        db <- .self$.selectByRelInt(db, min.rel.int)
    if (precursor)
        db <- .self$.selectByPrecursors(db)
    if (level > 0)
        db <- .self$.selectByMsLevel(db, level)

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

# Do search M/Z range {{{3
################################################################################

.doSearchMzRange=function(mz.min, mz.max, min.rel.int, ms.mode, max.results,
                          precursor, ms.level) {
    return(.self$.select(mz.min=mz.min, mz.max=mz.max, min.rel.int=min.rel.int,
                         mode=ms.mode, max.rows=max.results, cols='accession',
                         drop=TRUE, uniq=TRUE, sort=TRUE, precursor=precursor,
                         level=ms.level))
},

# Do get mz values {{{3
################################################################################

# Inherited from BiodbMassdbConn.
.doGetMzValues=function(ms.mode, max.results, precursor, ms.level) {

    # Get mz values
    mz <- .self$.select(cols='peak.mztheo', mode=ms.mode, drop=TRUE, uniq=TRUE,
                        sort=TRUE, max.rows=max.results, precursor=precursor,
                        level=ms.level)

    return(mz)
},

# Do set database data frame {{{3
################################################################################

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

# Check setting of URL {{{3
################################################################################

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

# Check if parsing has began {{{3
################################################################################

.checkParsingHasBegan=function() {

    # Parsing has began?
    if (length(.self$.parsing.expr) > 0)
        .self$error('Action impossible, parsing of entries has already began.')
},

# Get entry ids {{{3
################################################################################

.doGetEntryIds=function(max.results=NA_integer_) {

    ids <- NA_character_

    ids <- as.character(.self$.select(cols='accession', drop=TRUE, uniq=TRUE,
                                      sort=TRUE, max.rows=max.results))

    return(ids)
}

))
