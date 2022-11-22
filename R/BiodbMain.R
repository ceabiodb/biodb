#' The central class of the biodb package.
#'
#' The main class of the \code{biodb} package.
#' In order to use the biodb package, you need first to create an instance of
#' this class.
#'
#' The constructor takes a single argument, \code{autoloadExtraPkgs}, to enable
#' (\code{TRUE} or default) or disable (\code{FALSE}) autoloading of extra
#' biodb packages.
#'
#' Once the instance is created, some other important classes
#' (\code{BiodbFactory}, \code{BiodbPersistentCache}, \code{BiodbConfig}, ...)
#' are instantiated (just once) and their instances are later accessible through
#' get*() methods.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{BiodbPersistentCache}},
#' \code{\link{BiodbConfig}}, \code{\link{BiodbEntryFields}},
#' \code{\link{BiodbDbsInfo}}.
#'
#' @examples
#' # Create an instance:
#' mybiodb <- biodb::newInst()
#'
#' # Get the factory instance
#' fact <- mybiodb$getFactory()
#'
#' # Terminate instance.
#' mybiodb$terminate()
#' mybiodb <- NULL
#'
#' @import R6
#' @import yaml
#' @import plyr
#' @export
BiodbMain <- R6::R6Class("BiodbMain",

public=list(

#' @description
#' New instance initializer. The BiodbMain must not be instantiated directly.
#' Instead use the newInst() global method.
#' @param autoloadExtraPkgs Set to TRUE to allow automatic loading of extension
#' packages. Set to FALSE to forbid it. If left to NULL, the default,
#' autoload.extra.pkgs configuration value will be used.
#' @return Nothing.
initialize=function(autoloadExtraPkgs=NULL) {

    chk::chk_null_or(autoloadExtraPkgs, vld=chk::vld_flag)

    private$observers <- list()
    private$config <- NULL
    private$persistentCache <- NULL
    private$dbsinfo <- NULL
    private$factory <- NULL
    private$entry.fields <- NULL
    private$request.scheduler <- NULL

    # Load biodb definitions
    private$loadBiodbPkgsDefinitions(c(biodb=packageVersion('biodb')))
    
    # Load definitions from extra biodb packages

    pkgs <- private$listBiodbPkgsToLoad(autoloadExtraPkgs)
    private$loadBiodbPkgsDefinitions(pkgs)

    # Check locale
    private$checkLocale()

    logDebug('Created successfully new BiodbMain instance.')
    logDebug('This is biodb version %s.', packageVersion('biodb'))

    return(invisible(NULL))
},

#' @description
#' Closes \\code{BiodbMain} instance. Call this method when you are done with
#' your \\code{BiodbMain} instance.
#' @return Nothing.
terminate=function() {

    logInfo('Closing BiodbMain instance...')

    # Terminate factory
    if ( ! is.null(private$factory))
        private$factory$.__enclos_env__$private$terminate()

    # Terminate observers
    notifyObservers(private$observers, 'terminate')

    return(invisible(NULL))
},

#' @description
#' Loads databases and entry fields definitions from YAML file.
#' @param file The path to a YAML file containing definitions for
#' \\code{BiodbMain} (databases, fields or configuration keys).
#' @param package The package to which belong the new definitions.
#' @return Nothing.
loadDefinitions=function(file, package='biodb') {
    chk::chk_file(file)
    logDebug('Load definitions from file "%s".', file)

    # Load file
    def <- yaml::read_yaml(file)

    # Define config properties
    if ('config' %in% names(def))
        self$getConfig()$define(def$config)

    # Define databases
    if ('databases' %in% names(def))
        self$getDbsInfo()$define(def$databases, package=package)

    # Define fields
    if ('fields' %in% names(def))
        self$getEntryFields()$define(def$fields)

    return(invisible(NULL))
},

#' @description
#' Returns the single instance of the \\code{BiodbConfig} class.
#' @return The instance of the \\code{BiodbConfig} class attached to
#'     this BiodbMain instance.
getConfig=function() {

    if (is.null(private$config))
        private$config <- BiodbConfig$new(parent=self)

    return(private$config)
},

#' @description
#' Returns the single instance of the BiodbPersistentCache class.
#' @return The instance of the BiodbPersistentCache class attached to
#'     this BiodbMain instance.
getPersistentCache=function() {

    if (is.null(private$persistentCache)) {
        impl <- self$getConfig()$get('persistent.cache.impl')
        if (impl == 'bioc')
            private$persistentCache <-
                BiodbBiocPersistentCache$new(cfg=self$getConfig(), bdb=self)
        else # custom
            private$persistentCache <-
                BiodbCustomPersistentCache$new(cfg=self$getConfig(), bdb=self)
    }

    return(private$persistentCache)
},

#' @description
#' Returns the single instance of the \\code{BiodbDbsInfo} class.
#' @return The instance of the \\code{BiodbDbsInfo} class attached to
#'     this BiodbMain instance.
getDbsInfo=function() {

    if (is.null(private$dbsinfo))
        private$dbsinfo <- BiodbDbsInfo$new(cfg=self$getConfig())

    return(private$dbsinfo)
},

#' @description
#' Returns the single instance of the \\code{BiodbEntryFields} class.
#' @return The instance of the \\code{BiodbEntryFields} class
#'     attached to this BiodbMain instance.
getEntryFields=function() {

    if (is.null(private$entry.fields))
        private$entry.fields <- BiodbEntryFields$new(parent=self)

    return(private$entry.fields)
},

#' @description
#' Returns the single instance of the \\code{BiodbFactory} class.
#' @return The instance of the \\code{BiodbFactory} class attached to
#'     this BiodbMain instance.
getFactory=function() {

    if (is.null(private$factory))
        private$factory <- BiodbFactory$new(bdb=self)

    return(private$factory)
},

#' @description
#' Returns the single instance of the \\code{BiodbRequestScheduler}
#'     class.
#' @return The instance of the \\code{BiodbRequestScheduler} class
#'     attached to this BiodbMain instance.
getRequestScheduler=function() {

    if (is.null(private$request.scheduler))
        private$request.scheduler <- BiodbRequestScheduler$new(bdb=self)

    return(private$request.scheduler)
},

#' @description
#' Adds new observers. Observers will be called each time an event
#'     occurs. This is the way used in biodb to get feedback about what is going
#'     inside biodb code.
#' @param observers Either an object or a list of objects.
#' @return Nothing.
addObservers=function(observers) {

    # Make sure that input is a list
    if ( ! is.list(observers))
        observers <- list(observers)

    # Add observers to current list (insert at beginning)
    old_obs <- private$observers
    private$observers <- if (is.null(private$observers)) observers
        else c(observers, private$observers)

    # Notify of new observers
    notifyObservers(old_obs, 'notifyNewObservers', obs=observers)

    return(invisible(NULL))
},

#' @description
#' Gets the list of registered observers.
#' @return The list or registered observers.
getObservers=function() {

    return(private$observers)
},

#' @description
#' Gets the database class name corresponding to an entry ID field.
#' @param entry.id.field The name of an ID field. It must end with \".id\".
convertEntryIdFieldToDbClass=function(entry.id.field) {

    db.name <- NULL

    # Does it end with '.id'?
    m <- stringr::str_match(entry.id.field, '^(.*)\\.id$')[1, 2]
    if ( ! is.na(m) && self$getDbsInfo()$isDefined(m))
        db.name <- m

    return(db.name)
},

#' @description
#' Extracts the value of a field from a list of entries. Returns either a
#'     vector or a list depending on the type of the field.
#' @param entries A list of \\code{BiodbEntry} instances. 
#' @param field The name of a field.
#' @param flatten If set to \\code{TRUE} and the field has a cardinality
#' greater than one, then values be converted into a vector of class
#' character in which each entry values are collapsed.
#' @param compute If set to \\code{TRUE}, computable fields will be
#' output.
#' @param limit The maximum number of values to retrieve for each entry.
#' Set to 0 to get all values.
#' @param withNa If set to TRUE, keep NA values. Otherwise filter out NAs
#' values in vectors.
#' @return A vector if the field is atomic or flatten is set to
#' \\code{TRUE}, otherwise a list.
entriesFieldToVctOrLst=function(entries, field, flatten=FALSE, compute=TRUE,
    limit=0, withNa=TRUE) {

    val <- NULL

    # Vector
    if (self$getEntryFields()$get(field)$isVector()
        && (flatten || self$getEntryFields()$get(field)$hasCardOne())) {
        field.class <- self$getEntryFields()$get(field)$getClass()

        if (length(entries) > 0) {
            val <-lapply(entries,
                function(e) e$getFieldValue(field, flatten=flatten,
                    compute=compute, limit=limit, withNa=withNa))
            val <- unlist(val)
        }
        else
            val <- vector(mode=field.class, length=0)
    }

    # List
    else {
        if (length(entries) > 0)
            val <- lapply(entries, function(e) e$getFieldValue(field,
                compute=compute, limit=limit, withNa=withNa))
        else
            val <- list()
    }

    return(val)
},

#' @description
#' Converts a list of entries or a list of list of entries
#' (\\code{BiodbEntry} objects) into a data frame.
#' @param entries A list of \\code{BiodbEntry} instances or a list of list of
#' \\code{BiodbEntry} instances.
#' @param only.atomic If set to \\code{TRUE}, output only atomic fields, i.e.:
#' the fields whose value type is one of integer, numeric, logical or
#' character.
#' @param null.to.na If set to \\code{TRUE}, each \\code{NULL} entry in the
#' list is converted into a row of NA values.
#' @param compute If set to \\code{TRUE}, computable fields will be output.
#' @param fields A character vector of field names to output. The data frame
#' output will be restricted to this list of fields.
#' @param limit The maximum number of field values to write into new columns.
#' Used for fields that can contain more than one value. Set it to 0 to get all
#' values.
#' @param drop If set to \\code{TRUE} and the resulting data frame has only one
#' column, a vector will be output instead of data frame.
#' @param sort.cols Sort columns in alphabetical order.
#' @param flatten If set to \\code{TRUE}, then each field with a cardinality
#' greater than one, will be converted into a vector of class character whose
#' values are collapsed.
#' @param only.card.one Output only fields whose cardinality is one.
#' @param own.id If set to TRUE includes the database id field named
#' `<database_name>.id` whose values are the same as the `accession` field.
#' @param prefix Insert a prefix at the start of all field names.
#' @return A data frame containing the entries. Columns are named
#' according to field names.
entriesToDataframe=function(entries, only.atomic=TRUE, null.to.na=TRUE,
    compute=TRUE, fields=NULL, limit=0, drop=FALSE, sort.cols=FALSE,
    flatten=TRUE, only.card.one=FALSE, own.id=TRUE, prefix='') {

    chk::chk_list(entries)

    entries.df <- data.frame(stringsAsFactors=FALSE)

    if (length(entries) > 0 && (is.null(fields) || length(fields) > 0)) {

        logDebug("%d entrie(s) to convert in data frame.", length(entries))

        # Convert list of entries to a list of data frames.
        df.list <- private$entriesToListOfDataframes(entries=entries,
            only.atomic=only.atomic, compute=compute, fields=fields,
            flatten=flatten, limit=limit, only.card.one=only.card.one,
            own.id=own.id, null.to.na=null.to.na)

        # Build data frame of all entries
        if ( ! is.null(df.list)) {
            logDebug0("Merging data frames with a single",
                "entry each into a single data frame with all entries.")
            entries.df <- plyr::rbind.fill(df.list)
            if (is.null(colnames(entries.df)))
                colnames(entries.df) <- character()
        }
    }

    # Sort columns
    if (sort.cols && ncol(entries.df) > 1)
        entries.df <- entries.df[sort(colnames(entries.df))]

    # Add prefix
    if (ncol(entries.df) > 1 && ! is.na(prefix) && prefix != '') {
        fct <- function(x) substr(x, 1, nchar(prefix)) != prefix
        noprefix <- vapply(colnames(entries.df), fct, FUN.VALUE=TRUE)
        colnames(entries.df)[noprefix] <- paste0(prefix,
            colnames(entries.df)[noprefix])
    }

    # Drop
    if (drop && ncol(entries.df) == 1)
        entries.df <- entries.df[[1]]

    return(entries.df)
},

#' @description
#' Construct a data frame using entry IDs and field values of the
#' corresponding entries.
#' @param ids A character vector of entry IDs or a list of character vectors of
#' entry IDs.
#' @param db The biodb database name for the entry IDs, or a connector ID, as a
#' sinle character value.
#' @param fields A character vector containing entry fields to add.
#' @param limit The maximum number of field values to write into new columns.
#' Used for fields that can contain more than one value. Set it to 0 to get all
#' values.
#' @param own.id If set to TRUE includes the database id field named
#' `<database_name>.id` whose values are the same as the `accession` field.
#' @param prefix Insert a prefix at the start of all field names.
#' @param A data frame containing in columns the requested field
#' values, with one entry per line, in the same order than in `ids` vector.
entryIdsToDataframe=function(ids, db, fields=NULL, limit=3, prefix='',
    own.id=FALSE) {

    # Get connector
    conn <- if (is.character(db)) self$getFactory()$getConn(db) else
        chk::chk_is(db, 'BiodbConn')

    # Get entries
    if (is.character(ids))
        entries <- conn$getEntry(ids)
    else if (is.list(ids)) {
        entries <- list()
        for (i in ids) {
            e <- if (length(i) > 0) conn$getEntry(i) else list()
            entries <- c(entries, list(e))
        }
    }
    else
        error(paste("Input parameter `ids` must be either a",
            "character vector or a list of character vectors."))

    # Convert to data frame
    x <- self$entriesToDataframe(entries, fields=fields, limit=limit,
        prefix=prefix, drop=FALSE, own.id=own.id)

    return(x)
},

#' @description
#' Add values from a database to an existing data frame using a column
#' containing entry identifiers.
#' @param x A data frame containing at least one column with Biodb entry IDs
#' identified by the parameter `id.col`.
#' @param id.col The name of the column containing IDs inside the input data
#' frame.
#' @param db The biodb database name for the entry IDs, or a connector ID, as a
#' single character value.
#' @param fields A character vector containing entry fields to add.
#' @param limit The maximum number of field values to write into new columns.
#' Used for fields that can contain more than one value. Set it to 0 to get all
#' values.
#' @param prefix Insert a prefix at the start of all field names.
#' @return A data frame containing `x` and new columns appended for the fields
#' requested.
addColsToDataframe=function(x, id.col, db, fields, limit=3, prefix='') {
    
    chk::chk_is(x, 'data.frame')
    
    if (ncol(x) > 0) {
        chk::chk_character(id.col)
        if ( ! id.col %in% colnames(x))
            error('Column "%s" was not found inside data frame.', id.col)

        # Get ids
        ids <- as.character(x[[id.col]])

        # Get data frame of fields
        y <- self$entryIdsToDataframe(ids, db=db, fields=fields, limit=limit,
            prefix=prefix)

        # Merge data frames
        if (is.data.frame(y) && nrow(y) > 0)
            x <- cbind(x, y)
    }

    return(x)
},

#' @description
#' Converts a list of \\code{BiodbEntry} objects into JSON. Returns a vector of
#' characters.
#' @param entries A list of \\code{BiodbEntry} instances. It may contain NULL
#' elements.
#' @param compute If set to \\code{TRUE}, computable fields will added to JSON
#' too.
#' @return A list of JSON strings, the same length as entries list.
entriesToJson=function(entries, compute=TRUE) {

    chk::chk_list(entries)
    chk::chk_all(entries, chk::chk_null_or, vld=chk::vld_is, class='BiodbEntry')
    chk::chk_flag(compute)

    fct <- function(e) { 
        if (is.null(e))
            jsonlite::toJSON(e)
        else
            e$getFieldsAsJson(compute=compute)
    }
    json <- vapply(entries, fct, FUN.VALUE='')

    return(json)
},

#' @description
#' Collapses rows of a data frame, by looking for duplicated values in the
#' reference columns (parameter `cols`). The values contained in the reference
#' columns are supposed to be ordered inside the data frame, in the sens that
#' all duplicated values are supposed to directly follow the original values.
#' For all rows containing duplicated values, we look at values in all other
#' columns and concatenate values in each column containing different values.
#' @param x A data frame.
#' @param cols The indices or the names of the columns used as reference.
#' @param sep The separator to use when concatenating values in collapsed rows.
#' @return A data frame, with rows collapsed."
collapseRows=function(x, sep='|', cols=1L) {
    if (is.null(x))
        return(x)
    chk::chk_is(x, 'data.frame')
    if (nrow(x) == 0)
        return(x)
    if (is.numeric(cols))
        cols <- as.integer(cols)
    if ( ! is.integer(cols) && ! all(cols %in% colnames(x)))
        error('The data frame does not contain columns "%s".',
            paste(cols, collapse=', '))
    chk::chk_character(sep)

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

#' @description
#' Extract all values of a field from a list of entries.
#' @param entries A list of BiodbEntry objects.
#' @param field The field for which to extract values.
#' @param sortOutput Set to TRUE to sort the values.
#' @param uniq Set to TRUE to remove duplicates.
#' @return The values of the field as a vector.
entriesToSingleFieldValues=function(entries, field, sortOutput=FALSE,
    uniq=TRUE) {

    # Get values
    fct <- function(e) {
        e$getFieldValue(field)
    }
    values <- unlist(lapply(entries, fct))

    # Remove duplicates
    if (uniq)
        values <- unique(values)

    # Sort
    if (sortOutput)
        values <- sort(values)

    return(values)
},

#' @description
#' Extract all values of a field from a list of entries.
#' @param ids A list of entry identifiers.
#' @param db The database ID or connector ID where to find the entries.
#' @param field The field for which to extract values.
#' @param sortOutput Set to TRUE to sort the values.
#' @param uniq Set to TRUE to remove duplicates.
#' @return The values of the field as a vector.
entryIdsToSingleFieldValues=function(ids, db, field, sortOutput=FALSE,
    uniq=TRUE) {

    # Get connector
    conn <- self$getFactory()$getConn(db)

    # Get entries
    entries <- conn$getEntry(ids)

    # Call other method
    return(self$entriesToSingleFieldValues(entries, field=field,
        sortOutput=sortOutput, uniq=uniq))
},

#' @description
#' Computes missing fields in entries, for those fields that are
#'     comptable.
#' @param entries A list of \\code{BiodbEntry} instances. It may contain NULL
#'     elements.
#' @return Nothing.
computeFields=function(entries) {
    chk::chk_list(entries)
    chk::chk_all(entries, chk::chk_null_or, vld=chk::vld_is, class='BiodbEntry')

    # Loop on all entries
    for (e in entries)
        if ( ! is.null(e))
            e$computeFields()

    return(invisible(NULL))
},

#' @description
#' Saves a list of entries in JSON format. Each entry will be saved in a
#' separate file.
#' @param entries A list of \\code{BiodbEntry} instances. It may contain NULL
#' elements.
#' @param files A character vector of file paths, the same length as entries
#' list.
#' @param compute If set to \\code{TRUE}, computable fields will be saved too.
#' @return Nothing.
saveEntriesAsJson=function(entries, files, compute=TRUE) {

    chk::chk_list(entries)
    chk::chk_all(entries, chk::chk_null_or, vld=chk::vld_is, class='BiodbEntry')
    chk::chk_character(files)
    chk::chk_flag(compute)
    chk::chk_length(entries, length(files))

    # Save
    for (i in seq_along(entries))
        if ( ! is.null(entries[[i]])) {
            json <- entries[[i]]$getFieldsAsJson(compute=compute)
            writeChar(json, files[[i]], eos=NULL)
        }

    return(invisible(NULL))
},

#' @description
#' Copies all entries of a database into another database. The connector of the
#' destination database must be editable.
#' @param conn.from The connector of the source datababase to copy.
#' @param conn.to The connector of the destination database.
#' @param limit The number of entries of the source database to copy. If set to
#' \\code{NULL}, copy the whole database.
#' @return Nothing.
copyDb=function(conn.from, conn.to, limit=0) {

    # Get all entry IDs of "from" database
    ids <- conn.from$getEntryIds(max.results=limit)

    # Get entries
    entries <- conn.from$getEntry(ids)

    # Loop on all entries
    prg <- Progress$new(biodb=self, msg='Copying entries.',
        total=length(entries))
    for (entry in entries) {

        # Clone entry
        clone <- entry$cloneInstance(conn.to$getDbClass())

        # Add new entry
        conn.to$addNewEntry(clone)

        # Progress message
        prg$increment()
    }

    return(invisible(NULL))
},

#' @description
#' Prints object information.
#' @return Nothing.
print=function() {

    # Print version
    v <- as.character(packageVersion('biodb'))
    cat("BiodbMain instance, version ", v, ".\n", sep='')

    # List loaded connectors
    ids <- sort(self$getDbsInfo()$getIds())
    cat("Available connectors are: ", paste(ids, collapse=", "), ".\n", sep='')

    # List disabled connectors
    dbs <- self$getDbsInfo()$getAll()
    fct <- function(x) x$getPropertyValue('disabled')
    disabled <- vapply(dbs, fct, FUN.VALUE=TRUE)
    if (any(disabled)) {
        fct <- function(x) x$getPropertyValue('name')
        dbnames <- vapply(dbs[disabled], fct, FUN.VALUE='')
        cat("The following connectors are disabled: ",
            paste(dbnames, collapse=', '), ".\n", sep='')
    }

    return(invisible(NULL))
},

#' @description
#' DEPRECATED method to test if a field is an atomic field. The new
#' method is \\code{BiodbEntryField :isVector()}."
#' @param field The name of the field.
#' @return TRUE if the field's value is atomic.
fieldIsAtomic=function(field) {
    lifecycle::deprecate_warn('1.0.0', 'fieldIsAtomic()',
        'BiodbEntryField::isVector()')

    return(self$getEntryFields()$get(field)$isVector())
},

#' @description
#' DEPRECATED method to get the class of a field. The new method is
#' \code{BiodbMain :getEntryFields()$get(field)$getClass()}.
#' @param field The name of the field.
#' @return The class of the field.
getFieldClass=function(field) {
    lifecycle::deprecate_warn('1.0.0', 'getFieldClass()',
        'BiodbEntryField::getClass()')

    return(self$getEntryFields()$get(field)$getClass())
}
),

private=list(
    factory=NULL,
    observers=NULL,
    config=NULL,
    persistentCache=NULL,
    dbsinfo=NULL,
    entry.fields=NULL,
    request.scheduler=NULL,

listBiodbPkgsToLoad=function(autoloadExtraPkgs=NULL) {
    
    if (is.null(autoloadExtraPkgs))
        autoloadExtraPkgs <- self$getConfig()$get('autoload.extra.pkgs')

    if (autoloadExtraPkgs) {
        pkgs <- installed.packages()[, 'Version']
        pkgs <- pkgs[grep('^biodb[A-Z]', names(pkgs))]
        pkgs <- pkgs[unique(names(pkgs))] # Having twice the library name may
        # happen while building vignettes.
    } else
        pkgs <- character()

    return(pkgs)
},

loadBiodbPkgsDefinitions=function(pkgs) {
    for (pkg in sort(names(pkgs))) {
        logInfo('Loading definitions from package %s version %s.',
            pkg, pkgs[[pkg]])
        file <- system.file("definitions.yml", package=pkg)
        self$loadDefinitions(file, package=pkg)
    }
},

checkLocale=function() {

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
        if (private$config$isEnabled('force.locale'))
            tryCatch(expr={Sys.setlocale(locale='en_US.UTF-8')}, # Force locale
                warning=function(w) { NULL })
        else
            warn0("LC_CTYPE field of locale is set to ", LC_CTYPE,
                ". It must be set to a UTF-8 locale like",
                "'en_US.UTF-8'.")
    }
},

entriesToListOfDataframes=function(entries, only.atomic, compute, fields,
    flatten, limit, only.card.one, own.id, null.to.na, progress=TRUE) {

    df.list <- list()

    # Loop on all entries
    prg <- Progress$new(biodb=self, msg='Converting entries to data frame.',
        total=length(entries))
    for (e in entries) {

        e.df <- NULL

        # Send progress message
        if (progress)
            prg$increment()

        # List of entries
        if (is.list(e)) {
            # Get the list of data frames for those entries
            x <- private$entriesToListOfDataframes(e, only.atomic, compute,
                fields, flatten, limit, only.card.one, own.id, null.to.na,
                progress=FALSE)

            # Reduce these data frames to one data frame with one row.
            sep <- self$getConfig()$get('entries.sep')
            y <- plyr::rbind.fill(x)
            e.df <- y[c(), , drop=FALSE]
            for (k in colnames(y))
                e.df[1, k] <- paste(y[[k]], collapse=sep)
        }

        # Single entry
        else if (methods::is(e, "BiodbEntry")) {
            e.df <- e$getFieldsAsDataframe(only.atomic=only.atomic,
                compute=compute, fields=fields, flatten=flatten, limit=limit,
                only.card.one=only.card.one, own.id=own.id)
        }

        logTrace("Entry converted to data frame: %s.", df2str(e.df))
        df.list <- c(df.list, list(e.df))
    }

    # Replace NULL items with a data frame (empty or with NA values)
    nulls <- vapply(df.list, is.null, FUN.VALUE=TRUE)
    if (any(nulls)) {
        if (all(nulls) || ! null.to.na)
            x <- data.frame(stringsAsFactors=FALSE)
        else {
            x <- df.list[ ! nulls][[1]]
            x[,] <- NA
        }
        df.list[nulls] <- rep(list(x), sum(nulls))
    }

    logDebug("Converted %d entry/ies to data frame(s).", length(df.list))
    return(df.list)
}
))
