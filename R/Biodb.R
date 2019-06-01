# vi: fdm=marker ts=4 et cc=80

# Biodb {{{1
################################################################################

#' The central class of the biodb package.
#'
#' In order to use the biodb package, you need first to create an instance of
#' this class. See section Fields for a list of the constructor's parameters.
#'
#' @field logger    Set to \code{FALSE} if you want to disable the default
#"                  logger.
#' @field observers Either a \code{BiodbObserver} class instance or a list of
#'                  \code{BiodbObserver} class instances.
#'
#' @param compute     If set to \code{TRUE} and an entry has not the field
#'                    defined, then try to compute the field values.
#' @param entries     A list of \code{BiodbEntry} objects.
#' @param field       The name of a field.
#' @param files       A list of file paths.
#' @param flatten     If set to \code{TRUE} and the field has a cardinality
#'                    greater than one, then values are collapsed and output is
#'                    a vector of class character. 
#' @param null.to.na  If \code{TRUE}, each \code{NULL} entry is converted into a
#'                    line of \code{NA} values inside the data frame."
#' @param observers   Either a \code{BiodbObserver} class instance or a list of
#'                    \code{BiodbObserver} class instances.
#' @param only.atomic Set to \code{TRUE} if you want only the fields of atomic
#'                    type (\code{integer}, \code{numeric}, \code{logical} and
#'                    \code{character}) inside the data frame.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{BiodbCache}},
#' \code{\link{BiodbConfig}}, \code{\link{BiodbObserver}},
#' \code{\link{BiodbLogger}}, \code{\link{BiodbEntryFields}},
#' \code{\link{BiodbDbsInfo}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' # Create an instance without the default logger:
#' mybiodb <- biodb::Biodb(logger = FALSE)
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' # Create an instance with a file logger
#' mybiodb <- biodb::Biodb(logger = FALSE,
#'                         observers = biodb::BiodbLogger(file = "file.log"))
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @include BiodbObject.R
#' @export Biodb
#' @exportClass Biodb
Biodb <- methods::setRefClass("Biodb",
    contains = "BiodbObject",
    fields = list(
        .factory = "ANY",
        .observers = "ANY",
        .config = "ANY",
        .cache = "ANY",
        .entry.fields = "ANY",
        .dbsinfo = "ANY",
        .request.scheduler = "ANY"),

methods = list(

# Public methods {{{2
################################################################################

# Initialize {{{3
################################################################################

initialize = function(logger = TRUE, observers = NULL, ...) {

    callSuper(...)

    # Set observers
    .self$.observers <- list(BiodbWarningReporter$new(),
                             BiodbErrorReporter$new())
    if (logger)
        .self$addObservers(BiodbLogger$new())
    if ( ! is.null(observers))
        .self$addObservers(observers)

    # Print package version
    .self$message('info', paste0('This is biodb version ',
                                 packageVersion('biodb'), '.'))

    # Create instances of children
    .self$.config <- BiodbConfig$new(parent = .self)
    .self$.cache <- BiodbCache$new(parent = .self)
    .self$.dbsinfo <- BiodbDbsInfo$new(parent = .self)
    .self$.factory <- BiodbFactory$new(parent = .self)
    .self$.entry.fields <- BiodbEntryFields$new(parent = .self)
    .self$.request.scheduler <- BiodbRequestScheduler$new(parent = .self)

    # Load definitions
    file <- system.file("definitions.yml", package = "biodb")
    .self$loadDefinitions(file)

    # Check locale
    .self$.check.locale()

    .self$message('info', 'Created successfully new Biodb instance.')
},

# Terminate {{{3
################################################################################

terminate = function(logger = TRUE, observers = NULL, ...) {
    "Close \\code{Biodb} instance."

    .self$message('info', 'Closing Biodb instance.')

    # Terminate observers
    for (obs in .self$.observers)
        obs$terminate()

    # Terminate factory
    .self$.factory$.terminate()
},

# Load definitions {{{3
################################################################################

loadDefinitions = function(file) {
    'Load databases and entry fields definitions from YAML file.'

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

getConfig = function() {
    "Returns the single instance of the \\code{BiodbConfig} class."

    return(.self$.config)
},

# Get cache {{{3
################################################################################

getCache = function() {
    "Returns the single instance of the \\code{BiodbCache} class."

    return(.self$.cache)
},

# Get dbs info {{{3
################################################################################

getDbsInfo = function() {
    "Returns the single instance of the \\code{BiodbDbsInfo} class."

    return(.self$.dbsinfo)
},

# Get entry fields {{{3
################################################################################

getEntryFields = function() {
    "Returns the single instance of the \\code{BiodbEntryFields} class."

    return(.self$.entry.fields)
},

# Get factory {{{3
################################################################################

getFactory = function() {
    "Returns the single instance of the \\code{BiodbFactory} class."

    return(.self$.factory)
},

# Get request scheduler {{{3
################################################################################

getRequestScheduler = function() {
    "Returns the single instance of the \\code{BiodbRequestScheduler} class."

    return(.self$.request.scheduler)
},

# Add observers {{{3
################################################################################

addObservers = function(observers) {
    "Add new observers. Observers will be called each time an event occurs.
    This is the way used in biodb to get feedback about what is going inside
    biodb code."

    # Check types of observers
    if ( ! is.list(observers)) observers <- list(observers)
    is.obs <- vapply(observers, function(o) is(o, "BiodbObserver"),
                     FUN.VALUE = TRUE)
    if (any( ! is.obs))
        .self$message('error',
                      "Observers must inherit from BiodbObserver class.")

    # Add observers to current list (insert at beginning)
    .self$.observers <- if (is.null(.self$.observers)) observers
        else c(observers, .self$.observers)
},

# Get observers {{{3
################################################################################

getObservers = function() {
    "Get the list of registered observers."

    return(.self$.observers)
},

# Get biodb {{{3
################################################################################

getBiodb = function() {
    return(.self)
},

# Convert entry id field to database name {{{3
################################################################################

convertEntryIdFieldToDbClass = function(entry.id.field) {

    db.name <- NULL

    # Does it end with '.id'?
    m <- stringr::str_match(entry.id.field, '^(.*)\\.id$')[1, 2]
    if ( ! is.na(m) && .self$getDbsInfo()$isDefined(m))
        db.name <- m

    return(db.name)
},

# Entries field to vector or list {{{3
################################################################################

entriesFieldToVctOrLst = function(entries, field,
                                                 flatten = FALSE,
                                                 compute = TRUE) {
    "Extract the value of a field from a list of entries. Returns either a
    vector or a list depending on the type of the field."

    val <- NULL

    # Vector
    if (.self$getEntryFields()$get(field)$isVector()
        && (flatten || .self$getEntryFields()$get(field)$hasCardOne())) {
        field.class = .self$getEntryFields()$get(field)$getClass()

        if (length(entries) > 0) {
            val <-lapply(entries,
                         function(e)
                             e$getFieldValue(field, flatten = flatten,
                                             compute = compute))
            val <- unlist(val)
        }
        else
            val <- vector(mode = field.class, length = 0)
    }

    # List
    else {
        if (length(entries) > 0)
            val <- lapply(entries,
                          function(e) e$getFieldValue(field, compute = compute))
        else
            val <- list()
    }

    return(val)
},

# Entries to data frame {{{3
################################################################################

entriesToDataframe = function(entries, only.atomic = TRUE,
                                             null.to.na = TRUE, compute = TRUE,
                                             fields = NULL, drop = FALSE,
                                             sort.cols = FALSE, flatten = TRUE,
                                             only.card.one = FALSE) {
    "Convert a list of entries (\\code{BiodbEntry} objects) into a data frame."

    if ( ! is.list(entries))
        .self$message('error', "Parameter 'entries' must be a list.")

    entries.df <- NULL

    if (length(entries) > 0) {

        .self$message('debug', paste(length(entries),
                                     "entrie(s) to convert in data frame."))

        # Check classes
        if ( ! all(vapply(entries,
                          function(x) is.null(x) || is(x, 'BiodbEntry'),
                          FUN.VALUE = TRUE)))
            .self$message('error', paste("Some objects in the input list",
                                         "are not a subclass of BiodbEntry."))

        # Loop on all entries
        i = 0
        df.list <- NULL
        for (e in entries) {

            # Send progress message
            i = i + 1
            msg <- 'Converting entries to data frame.'
            .self$.sendProgress(msg=msg, index=i, total=length(entries),
                                first=(i == 1))

            e.df <- NULL
            if ( ! is.null(e))
                e.df <- e$getFieldsAsDataFrame(only.atomic = only.atomic,
                                               compute = compute,
                                               fields = fields,
                                               flatten = flatten,
                                               only.card.one = only.card.one)
            else if (null.to.na)
                e.df <- data.frame(ACCESSION = NA_character_)

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

entriesToJson = function(entries, compute = TRUE) {
    "Convert a list of BiodbEntry objects into JSON. Returns a vector of
    characters."

    json <- vapply(entries, function(e) e$getFieldsAsJson(compute = compute),
                   FUN.VALUE = '')

    return(json)
},

# Compute fields {{{3
################################################################################

computeFields = function(entries) {
    "Compute missing fields in entries."

    # Loop on all entries
    for (e in entries)
        e$computeFields()
},

# Save entries as JSON {{{3
################################################################################

saveEntriesAsJson = function(entries, files, compute = TRUE) {
    "Save a list of entries in JSON format."

    .self$.assert.equal.length(entries, files)

    # Save
    for (i in seq_along(entries)) {
        json <- entries[[i]]$getFieldsAsJson(compute = compute)
        writeChar(json, files[[i]])
    }

    return(json)
},

# Copy database {{{3
################################################################################

copyDb = function(conn.from, conn.to, limit = NULL) {
    "Copy all entries of a database into another database. The connector of the
    destination database must be editable."
    
    # Get all entry IDs of "from" database
    ids = conn.from$getEntryIds(max.results = limit)

    # Get entries
    entries = conn.from$getEntry(ids)

    # Loop on all entries
    i = 0
    for (entry in entries) {

        # Clone entry
        clone = entry$clone(conn.to$getDbClass())

        # Add new entry
        conn.to$addNewEntry(clone)

        # Send progress message
        i = i + 1
        msg <- 'Copying entries.'
        .self$.sendProgress(msg=msg, index=i, total=length(ids), first=(i == 1))
    }
},

# Show {{{3
################################################################################

show = function() {
    'Print object information.'
    
    v <- as.character(packageVersion('biodb'))
    cat("Biodb instance, version ", v, ".\n", sep = '')
},

# Private methods {{{2
################################################################################

# Send progress message {{{3
################################################################################

.sendProgress = function(msg, index, total, first) {
    lapply(.self$getObservers(),
           function(x) x$progress(type='info', msg=msg, index=index,
                                  total=total, first=first))
},

# Check locale {{{3
################################################################################

.check.locale = function() {

    # Get locale
    locale <- Sys.getlocale()
    locale.split <- strsplit(strsplit(Sys.getlocale(), ';')[[1]], '=')
    if (length(locale.split) == 1)
        LC_CTYPE <- locale.split[[1]][[1]]
    else {
        keys <- vapply(locale.split, function(x) x[[1]], FUN.VALUE = '')
        locale.values <- vapply(locale.split, function(x) x[[2]],
                                FUN.VALUE = '')
        names(locale.values) <- keys
        LC_CTYPE <- locale.values[['LC_CTYPE']]
    }

    # Check LC_CTYPE
    if (length(grep('\\.utf-8$', tolower(LC_CTYPE))) == 0) {
        if (.self$.config$isEnabled('force.locale'))
            Sys.setlocale(locale = 'en_US.UTF-8') # Force locale
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

fieldIsAtomic = function(field) {

    .self$.deprecated.method('BiodbEntryField::isVector()')

    return(.self$getEntryFields()$get(field)$isVector())
},

# Get field class {{{3
################################################################################

getFieldClass = function(field) {

    .self$.deprecated.method('Biodb::getEntryFields()$get(field)$getClass()')

    return(.self$getBiodb()$getEntryFields()$get(field)$getClass())
}

))
