# vi: fdm=marker ts=4 et cc=80 tw=80

# Biodb {{{1
################################################################################

#' The central class of the biodb package.
#'
#' The main class of the \code{biodb} package.
#' In order to use the biodb package, you need first to create an instance of
#' this class.
#'
#' The constructor takes no argument.
#'
#' Once the instance is created, some other important classes
#' (\code{BiodbFactory}, \code{BiodbCache}, \code{BiodbConfig}, ...) are
#' instantiated (just once) and their instances are later accessible through
#' get*() methods.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{BiodbCache}},
#' \code{\link{BiodbConfig}}, \code{\link{BiodbObserver}},
#' \code{\link{BiodbLogger}}, \code{\link{BiodbEntryFields}},
#' \code{\link{BiodbDbsInfo}}.
#'
#' @examples
#' # Create an instance:
#' mybiodb <- biodb::Biodb()
#'
#' # Get the factory instance
#' fact <- mybiodb$getFactory()
#'
#' # Terminate instance.
#' mybiodb$terminate()
#' mybiodb <- NULL
#'
#' @import methods
#' @include BiodbObject.R
#' @export Biodb
#' @exportClass Biodb
Biodb <- methods::setRefClass("Biodb",
    contains="BiodbObject",
    fields=list(
        .factory="ANY",
        .observers="ANY",
        .config="ANY",
        .cache="ANY",
        .entry.fields="ANY",
        .dbsinfo="ANY",
        .request.scheduler="ANY"),

methods=list(

# Public methods {{{2
################################################################################

# Initialize {{{3
################################################################################

initialize=function() {

    # Set default observers
    .self$.observers <- list(BiodbWarningReporter$new(),
                             BiodbErrorReporter$new(),
                             BiodbInfoReporter$new())

    # Print package version
    .self$info('This is biodb version ', packageVersion('biodb'), '.')

    # Create instances of children
    .self$.config <- BiodbConfig$new(parent=.self)
    .self$.cache <- BiodbCache$new(parent=.self)
    .self$.dbsinfo <- BiodbDbsInfo$new(parent=.self)
    .self$.factory <- BiodbFactory$new(parent=.self)
    .self$.entry.fields <- BiodbEntryFields$new(parent=.self)
    .self$.request.scheduler <- BiodbRequestScheduler$new(parent=.self)

    # Load definitions
    file <- system.file("definitions.yml", package="biodb")
    .self$loadDefinitions(file)

    # Check locale
    .self$.checkLocale()

    .self$info('Created successfully new Biodb instance.')
},

# Terminate {{{3
################################################################################

terminate=function() {
    ":\n\nCloses \\code{Biodb} instance. Call this method when you are done with
    your \\code{Biodb} instance.
    \nReturned value: None.
    "

    .self$info('Closing Biodb instance...')

    # Terminate factory
    .self$.factory$.terminate()

    # Terminate observers
    for (obs in .self$.observers)
        obs$terminate()
},

# Load definitions {{{3
################################################################################

loadDefinitions=function(file) {
    ":\n\nLoads databases and entry fields definitions from YAML file.
    \nfile: The path to a YAML file containing definitions for \\code{Biodb}
    (databases, fields or configuration keys).
    \nReturned value: None.
    "

    .self$debug('Load definitions from file "', file, '".')

    # Load file
    def <- yaml::read_yaml(file)

    # Define config properties
    if ('config' %in% names(def))
        .self$getConfig()$define(def$config)

    # Define databases
    if ('databases' %in% names(def))
        .self$getDbsInfo()$define(def$databases)

    # Define fields
    if ('fields' %in% names(def))
        .self$getEntryFields()$define(def$fields)
},

# Get configuration {{{3
################################################################################

getConfig=function() {
    ":\n\nReturns the single instance of the \\code{BiodbConfig} class.
    \nReturned value: The instance of the \\code{BiodbConfig} class attached to
    this Biodb instance.
    "

    return(.self$.config)
},

# Get cache {{{3
################################################################################

getCache=function() {
    ":\n\nReturns the single instance of the \\code{BiodbCache} class.
    \nReturned value: The instance of the \\code{BiodbCache} class attached to
    this Biodb instance.
    "

    return(.self$.cache)
},

# Get dbs info {{{3
################################################################################

getDbsInfo=function() {
    ":\n\nReturns the single instance of the \\code{BiodbDbsInfo} class.
    \nReturned value: The instance of the \\code{BiodbDbsInfo} class attached to
    this Biodb instance.
    "

    return(.self$.dbsinfo)
},

# Get entry fields {{{3
################################################################################

getEntryFields=function() {
    ":\n\nReturns the single instance of the \\code{BiodbEntryFields} class.
    \nReturned value: The instance of the \\code{BiodbEntryFields} class
    attached to this Biodb instance.
    "

    return(.self$.entry.fields)
},

# Get factory {{{3
################################################################################

getFactory=function() {
    ":\n\nReturns the single instance of the \\code{BiodbFactory} class.
    \nReturned value: The instance of the \\code{BiodbFactory} class attached to
    this Biodb instance.
    "

    return(.self$.factory)
},

# Get request scheduler {{{3
################################################################################

getRequestScheduler=function() {
    ":\n\nReturns the single instance of the \\code{BiodbRequestScheduler} class.
    \nReturned value: The instance of the \\code{BiodbRequestScheduler} class
    attached to this Biodb instance.
    "

    return(.self$.request.scheduler)
},

# Add observers {{{3
################################################################################

addObservers=function(observers) {
    ":\n\nAdds new observers. Observers will be called each time an event occurs.
    This is the way used in biodb to get feedback about what is going inside
    biodb code.
    \nobservers: Either a \\code{BiodbObserver} instance or a list of
    \\code{BiodbObserver} instances.
    \nReturned value: None.
    "

    # Check types of observers
    if ( ! is.list(observers)) observers <- list(observers)
    is.obs <- vapply(observers, function(o) methods::is(o, "BiodbObserver"),
                     FUN.VALUE=TRUE)
    if (any( ! is.obs))
        .self$message('error',
                      "Observers must inherit from BiodbObserver class.")

    # Add observers to current list (insert at beginning)
    old_obs <- .self$.observers
    .self$.observers <- if (is.null(.self$.observers)) observers
        else c(observers, .self$.observers)

    # Notify of new observers
    for(no in observers)
        lapply(old_obs, function(o) o$newObserver(no))
},

# Get observers {{{3
################################################################################

getObservers=function() {
    ":\n\nGets the list of registered observers.
    \nReturned value: The list or registered observers.
    "

    return(.self$.observers)
},

# Get biodb {{{3
################################################################################

getBiodb=function() {
    ":\n\nReturns the biodb instance, which is itself in the case of the
    \\code{Biodb} class.
    \nReturned value: This instance.
    "

    return(.self)
},

# Convert entry id field to database name {{{3
################################################################################

convertEntryIdFieldToDbClass=function(entry.id.field) {
    ":\n\nGets the database class name corresponding to an entry ID field.
    \nentry.id.field: The name of an ID field. It must end with \".id\".
    "

    db.name <- NULL

    # Does it end with '.id'?
    m <- stringr::str_match(entry.id.field, '^(.*)\\.id$')[1, 2]
    if ( ! is.na(m) && .self$getDbsInfo()$isDefined(m))
        db.name <- m

    return(db.name)
},

# Entries field to vector or list {{{3
################################################################################

entriesFieldToVctOrLst=function(entries, field, flatten=FALSE, compute=TRUE) {
    ":\n\nExtracts the value of a field from a list of entries. Returns either a
    vector or a list depending on the type of the field.
    \nentries: A list of \\code{Biodb} entries.
    \nfield: The name of a field.
    \nflatten: If set to \\code{TRUE} and the field has a cardinality greater
    than one, then values be converted into a vector of class character in which
    each entry values are collapsed.
    \ncompute: If set to \\code{TRUE}, computable fields will be output.
    \nReturned value: A vector if the field is atomic or flatten is set to
    \\code{TRUE}, otherwise a list.
    "

    val <- NULL

    # Vector
    if (.self$getEntryFields()$get(field)$isVector()
        && (flatten || .self$getEntryFields()$get(field)$hasCardOne())) {
        field.class <- .self$getEntryFields()$get(field)$getClass()

        if (length(entries) > 0) {
            val <-lapply(entries,
                         function(e)
                             e$getFieldValue(field, flatten=flatten,
                                             compute=compute))
            val <- unlist(val)
        }
        else
            val <- vector(mode=field.class, length=0)
    }

    # List
    else {
        if (length(entries) > 0)
            val <- lapply(entries,
                          function(e) e$getFieldValue(field, compute=compute))
        else
            val <- list()
    }

    return(val)
},

# Entries to data frame {{{3
################################################################################

entriesToDataframe=function(entries, only.atomic=TRUE,
                            null.to.na=TRUE, compute=TRUE,
                            fields=NULL, drop=FALSE,
                            sort.cols=FALSE, flatten=TRUE,
                            only.card.one=FALSE) {
    ":\n\nConverts a list of entries (\\code{BiodbEntry} objects) into a data
    frame.
    \nentries: A list of \\code{BiodbEntry} instances.
    \nonly.atomic: If set to \\code{TRUE}, output only atomic fields, i.e.: the
    fields whose value type is one of integer, numeric, logical or character.
    \nnull.to.na: If set to \\code{TRUE}, each \\code{NULL} entry in the list is
    converted into a row of NA values.
    \ncompute: If set to \\code{TRUE}, computable fields will be output.
    \nfields: A character vector of field names to output. The data frame output
    will be restricted to this list of fields.
    \ndrop: If set to \\code{TRUE} and the resulting data frame has only one
    column, a vector will be output instead of data frame.
    \nsort.cols: Sort columns in alphabetical order.
    \nflatten: If set to \\code{TRUE}, then each field with a cardinality
    greater than one, will be converted into a vector of class character whose
    values are collapsed.
    \nonly.card.one: Output only fields whose cardinality is one.
    \nReturned value: A data frame containing the entries. Columns are named
    according to field names.
    "

    if ( ! is.list(entries))
        .self$message('error', "Parameter 'entries' must be a list.")

    entries.df <- NULL

    if (length(entries) > 0) {

        .self$message('debug', paste(length(entries),
                                     "entrie(s) to convert in data frame."))

        # Check classes
        if ( ! all(vapply(entries,
                          function(x) is.null(x) || is(x, 'BiodbEntry'),
                          FUN.VALUE=TRUE)))
            .self$message('error', paste("Some objects in the input list",
                                         "are not a subclass of BiodbEntry."))

        # Loop on all entries
        i <- 0
        df.list <- NULL
        for (e in entries) {

            # Send progress message
            i <- i + 1
            msg <- 'Converting entries to data frame.'
            .self$.sendProgress(msg=msg, index=i, total=length(entries),
                                first=(i == 1))

            e.df <- NULL
            if ( ! is.null(e))
                e.df <- e$getFieldsAsDataFrame(only.atomic=only.atomic,
                                               compute=compute,
                                               fields=fields,
                                               flatten=flatten,
                                               only.card.one=only.card.one)
            else if (null.to.na)
                e.df <- data.frame(ACCESSION=NA_character_)

            if ( ! is.null(e.df))
                df.list <- c(df.list, list(e.df))
        }

        # Build data frame of all entries
        if ( ! is.null(df.list)) {
            .self$message('debug', paste("Merging data frames with a single",
                                         "entry each into a single data frame",
                                         "with all entries."))
            entries.df <- plyr::rbind.fill(df.list)
        }
    }

    # Sort columns
    if (sort.cols)
        entries.df <- entries.df[sort(colnames(entries.df))]

    # Drop
    if (drop && ! is.null(entries.df) && ncol(entries.df) == 1)
        entries.df <- entries.df[[1]]

    return(entries.df)
},

# Entries to JSON {{{3
################################################################################

entriesToJson=function(entries, compute=TRUE) {
    ":\n\nConverts a list of \\code{BiodbEntry} objects into JSON. Returns a
    vector of characters.
    \nentries: A list of \\code{BiodbEntry} instances.
    \ncompute: If set to \\code{TRUE}, computable fields will added to JSON too.
    \nReturned value: A list of JSON strings, the same length as entries list.
    "

    json <- vapply(entries, function(e) e$getFieldsAsJson(compute=compute),
                   FUN.VALUE='')

    return(json)
},

# Collapse rows {{{3
################################################################################

collapseRows=function(x, sep='|', cols=1L) {
    ":\n\nCollapses rows of a data frame, by looking for duplicated values in the
    reference columns (parameter `cols`). The values contained in the reference
    columns are supposed to be ordered inside the data frame, in the sens that
    all duplicated values are supposed to directly follow the original values.
    For all rows containing duplicated values, we look at values in all other
    columns and concatenate values in each column containing different values.
    \nx: A data frame.
    \ncols: The indices or the names of the columns used as reference.
    \nsep: The separator to use when concatenating values in collapsed rows.
    \nReturned value: A data frame, with rows collapsed."

    if (is.null(x))
        return(x)
    .self$.assertIs(x, 'data.frame')
    if (nrow(x) == 0)
        return(x)
    if (is.numeric(cols))
        cols <- as.integer(cols)
    if ( ! is.integer(cols) && ! all(cols %in% colnames(x)))
        .self$error('The data frame does not contain columns "',
                    paste(cols, collapse=', '), '".')
    .self$.assertIs(sep, 'character')

    y <- NULL

    # Get duplicated rows
    na.row <- apply(is.na(x[, cols, drop=FALSE]), 1, function(v) Reduce("|", v))
    dup.row <- ( ! na.row) & duplicated(x[, cols])

    # Loop on all rows
    i <- 1
    while (i <= length(dup.row)) {

        # Find end of block
        j <- i
        while (j < length(dup.row) && dup.row[[j + 1]])
            j <- j + 1

        # Collapse gathered lines
        one.line <- x[i, , drop=FALSE]
        if (j > i)
            for (col in colnames(x)) {
                if (( ! all(is.na(x[i:j, col]))
                     && any(is.na(x[i:j, col])))
                || ( ( ! is.na(one.line[[col]]))
                    && any(x[i:j, col] != one.line[[col]])))
                    one.line[[col]] <- paste(x[i:j, col], collapse=sep)
            }

        # Append collapsed line to output data frame
        y <- rbind(y, one.line)

        i <- j + 1
    }

    return(y)
},

# Compute fields {{{3
################################################################################

computeFields=function(entries) {
    ":\n\nComputes missing fields in entries, for those fields that are
    comptable.
    \nentries: A list of \\code{BiodbEntry} instances.
    \nReturned value: None.
    "

    # Loop on all entries
    for (e in entries)
        e$computeFields()
},

# Save entries as JSON {{{3
################################################################################

saveEntriesAsJson=function(entries, files, compute=TRUE) {
    ":\n\nSaves a list of entries in JSON format. Each entry will be saved in a
    separate file.
    \nentries: A list of \\code{BiodbEntry} instances.
    \nfiles: A list of file paths, the same length as entries list.
    \ncompute: If set to \\code{TRUE}, computable fields will be saved too.
    \nReturned value: None.
    "

    .self$.assertEqualLength(entries, files)

    # Save
    for (i in seq_along(entries)) {
        json <- entries[[i]]$getFieldsAsJson(compute=compute)
        writeChar(json, files[[i]])
    }
},

# Copy database {{{3
################################################################################

copyDb=function(conn.from, conn.to, limit=NULL) {
    ":\n\nCopies all entries of a database into another database. The connector
    of the destination database must be editable.
    \nconn.from: The connector of the source datababase to copy.
    \nconn.to: The connector of the destination database.
    \nlimit: The number of entries of the source database to copy. If set to
    \\code{NULL}, copy the whole database.
    \nReturned value: None.
    "

    # Get all entry IDs of "from" database
    ids <- conn.from$getEntryIds(max.results=limit)

    # Get entries
    entries <- conn.from$getEntry(ids)

    # Loop on all entries
    i <- 0
    for (entry in entries) {

        # Clone entry
        clone <- entry$clone(conn.to$getDbClass())

        # Add new entry
        conn.to$addNewEntry(clone)

        # Send progress message
        i <- i + 1
        msg <- 'Copying entries.'
        .self$.sendProgress(msg=msg, index=i, total=length(ids), first=(i == 1))
    }
},

# Show {{{3
################################################################################

show=function() {
    'Prints object information.'

    v <- as.character(packageVersion('biodb'))
    cat("Biodb instance, version ", v, ".\n", sep='')
},

# Private methods {{{2
################################################################################

# Send progress message {{{3
################################################################################

.sendProgress=function(msg, index, total, first) {
    lapply(.self$getObservers(),
           function(x) x$progress(type='info', msg=msg, index=index,
                                  total=total, first=first))
},

# Check locale {{{3
################################################################################

.checkLocale=function() {

    # Get locale
    locale <- Sys.getlocale()
    locale.split <- strsplit(strsplit(Sys.getlocale(), ';')[[1]], '=')
    if (length(locale.split) == 1)
        LC_CTYPE <- locale.split[[1]][[1]]
    else {
        keys <- vapply(locale.split, function(x) x[[1]], FUN.VALUE='')
        locale.values <- vapply(locale.split, function(x) x[[2]],
                                FUN.VALUE='')
        names(locale.values) <- keys
        LC_CTYPE <- locale.values[['LC_CTYPE']]
    }

    # Check LC_CTYPE
    if (length(grep('\\.utf-8$', tolower(LC_CTYPE))) == 0) {
        if (.self$.config$isEnabled('force.locale'))
            Sys.setlocale(locale='en_US.UTF-8') # Force locale
        else
            .self$message('warning',
                          paste("LC_CTYPE field of locale is set to ", LC_CTYPE,
                                ". It must be set to a UTF-8 locale like",
                                "'en_US.UTF-8'."))
    }
},

# Deprecated methods {{{2
################################################################################

# Field is atomic {{{3
################################################################################

fieldIsAtomic=function(field) {
    ":\n\nDEPRECATED method to test if a field is an atomic field. The new
    method is \\code{BiodbEntryField::isVector()}."

    .self$.deprecatedMethod('BiodbEntryField::isVector()')

    return(.self$getEntryFields()$get(field)$isVector())
},

# Get field class {{{3
################################################################################

getFieldClass=function(field) {
    ":\n\nDEPRECATED method to get the class of a field. The new method is
    \\code{Biodb::getEntryFields()$get(field)$getClass()}."

    .self$.deprecatedMethod('Biodb::getEntryFields()$get(field)$getClass()')

    return(.self$getBiodb()$getEntryFields()$get(field)$getClass())
}

))
