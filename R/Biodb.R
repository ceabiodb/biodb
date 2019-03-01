# vi: fdm=marker

# Class declaration {{{1
################################################################

#' The central class of the biodb package.
#'
#' In order to use the biodb package, you need first to create an instance of this class. See section Fields for a list of the constructor's parameters.
#'
#' @field logger    Set to \code{FALSE} if you want to disable the default logger.
#' @field observers Either a \code{BiodbObserver} class instance or a list of \code{BiodbObserver} class instances.
#'
#' @param compute     If set to \code{TRUE} and an entry has not the field defined, then try to compute the field values.
#' @param entries     A list of \code{BiodbEntry} objects.
#' @param field       The name of a field.
#' @param files       A list of file paths.
#' @param flatten     If set to \code{TRUE} and the field has a cardinality greater than one, then values are collapsed and output is a vector of class character. 
#' @param null.to.na  If \code{TRUE}, each \code{NULL} entry is converted into a line of \code{NA} values inside the data frame."
#' @param observers Either a \code{BiodbObserver} class instance or a list of \code{BiodbObserver} class instances.
#' @param only.atomic Set to \code{TRUE} if you want only the fields of atomic type (\code{integer}, \code{numeric}, \code{logical} and \code{character}) inside the data frame.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{BiodbCache}}, \code{\link{BiodbConfig}}, \code{\link{BiodbObserver}}, \code{\link{BiodbLogger}}, \code{\link{BiodbEntryFields}}, \code{\link{BiodbDbsInfo}}.
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
#' mybiodb <- biodb::Biodb(logger = FALSE, observers = biodb::BiodbLogger(file = "file.log"))
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @include BiodbObject.R
#' @export Biodb
#' @exportClass Biodb
Biodb <- methods::setRefClass("Biodb", contains = "BiodbObject", fields = list( .factory = "ANY", .observers = "ANY", .config = "ANY", .cache = "ANY", .entry.fields = "ANY", .dbsinfo = "ANY", .request.scheduler = "ANY", .entry.id.field.to.db.name = 'list'))

# Constructor {{{1
################################################################

Biodb$methods( initialize = function(logger = TRUE, observers = NULL, ...) {

	callSuper(...)

	# Set observers
	.observers <<- list(WarningReporter$new(), ErrorReporter$new())
	if (logger)
		.self$addObservers(BiodbLogger$new())
	if ( ! is.null(observers))
		.self$addObservers(observers)

	# Print package version
	.self$message('info', paste0('This is biodb version ', packageVersion('biodb'), '.'))

	# Create configuration instance
	.config <<- BiodbConfig$new(parent = .self)

	# Create cache
	.cache <<- BiodbCache$new(parent = .self)

	# Create databases information
	.dbsinfo <<- BiodbDbsInfo$new(parent = .self)

	# Create factory
	.factory <<- BiodbFactory$new(parent = .self)

	# Create entry fields
	.entry.fields <<- BiodbEntryFields$new(parent = .self)

	# Create scheduler
	.request.scheduler <<- BiodbRequestScheduler$new(parent = .self)

	# Initialize other fields
	.entry.id.field.to.db.name <<- list()

	# Check locale
	.self$.check.locale()

	.self$message('info', 'Created successfully new Biodb instance.')
})

# Terminate {{{1
################################################################

Biodb$methods( terminate = function(logger = TRUE, observers = NULL, ...) {
	":\n\nClose \\code{Biodb} instance."

	.self$message('info', 'Closing Biodb instance.')

	# Terminate observers
	for (obs in .self$.observers)
		obs$terminate()

	# Terminate factory
	.self$.factory$.terminate()
})

# Get configuration {{{1
################################################################

Biodb$methods( getConfig = function() {
	":\n\nReturns the single instance of the \\code{BiodbConfig} class."

	return(.self$.config)
})

# Get cache {{{1
################################################################

Biodb$methods( getCache = function() {
	":\n\nReturns the single instance of the \\code{BiodbCache} class."

	return(.self$.cache)
})

# Get dbs info {{{1
################################################################

Biodb$methods( getDbsInfo = function() {
	":\n\nReturns the single instance of the \\code{BiodbDbsInfo} class."

	return(.self$.dbsinfo)
})

# Get entry fields {{{1
################################################################

Biodb$methods( getEntryFields = function() {
	":\n\nReturns the single instance of the \\code{BiodbEntryFields} class."

	return(.self$.entry.fields)
})

# Get factory {{{1
################################################################

Biodb$methods( getFactory = function() {
	":\n\nReturns the single instance of the \\code{BiodbFactory} class."

	return(.self$.factory)
})

# Get request scheduler {{{1
################################################################

Biodb$methods( getRequestScheduler = function() {
	":\n\nReturns the single instance of the \\code{BiodbRequestScheduler} class."

	return(.self$.request.scheduler)
})

# Add observers {{{1
################################################################

Biodb$methods( addObservers = function(observers) {
	":\n\nAdd new observers. Observers will be called each time an event occurs. This is the way used in biodb to get feedback about what is going inside biodb code."

	# Check types of observers
	if ( ! is.list(observers)) observers <- list(observers)
	is.obs <- vapply(observers, function(o) is(o, "BiodbObserver"), FUN.VALUE = TRUE)
	if (any( ! is.obs))
		.self$message('error', "Observers must inherit from BiodbObserver class.")

	# Add observers to current list (insert at beginning)
	.observers <<- if (is.null(.self$.observers)) observers else c(observers, .self$.observers)
})

# Get observers {{{1
################################################################

Biodb$methods( getObservers = function() {
	":\n\nGet the list of registered observers."

	return(.self$.observers)
})

# Get biodb {{{1
################################################################

Biodb$methods( getBiodb = function() {
	return(.self)
})

# Convert entry id field to database name {{{1
################################################################

Biodb$methods( convertEntryIdFieldToDbClass = function(entry.id.field) {

	db.name <- NULL

	# Does it end with '.id'?
	m <- stringr::str_match(entry.id.field, '^(.*)\\.id$')[1, 2]
	if ( ! is.na(m) && .self$getDbsInfo()$isDefined(m))
		db.name <- m

	return(db.name)
})

# Entries field to vector or list {{{1
################################################################

Biodb$methods( entriesFieldToVctOrLst = function(entries, field, flatten = FALSE, compute = TRUE) {
	":\n\nExtract the value of a field from a list of entries.\n\nReturns either a vector or a list depending on the type of the field."

	val <- NULL

	# Vector
	if (.self$getEntryFields()$get(field)$isVector() && (flatten || .self$getEntryFields()$get(field)$hasCardOne())) {
		field.class = .self$getEntryFields()$get(field)$getClass()

		if (length(entries) > 0)
			val <- unlist(lapply(entries, function(e) e$getFieldValue(field, flatten = flatten, compute = compute)))
		else
			val <- vector(mode = field.class, length = 0)
	}

	# List
	else {
		if (length(entries) > 0)
			val <- lapply(entries, function(e) e$getFieldValue(field, compute = compute))
		else
			val <- list()
	}

	return(val)
})

# Entries to data frame {{{1
################################################################

Biodb$methods( entriesToDataframe = function(entries, only.atomic = TRUE, null.to.na = TRUE, compute = TRUE, fields = NULL, drop = FALSE, sort.cols = FALSE, flatten = TRUE, only.card.one = FALSE) {
	":\n\nConvert a list of entries (\\code{BiodbEntry} objects) into a data frame."

	if ( ! is.list(entries))
		.self$message('error', "Parameter 'entries' must be a list.")

	entries.df <- NULL

	if (length(entries) > 0) {

		.self$message('debug', paste(length(entries), "entrie(s) to convert in data frame."))

		# Check classes
		if ( ! all(vapply(entries, function(x) is.null(x) || is(x, 'BiodbEntry'), FUN.VALUE = TRUE)))
			.self$message('error', "Some objects in the input list are not a subclass of BiodbEntry.")

		# Loop on all entries
		n <- 0
		df.list <- NULL
		for (e in entries) {

			n <- n + 1
			.self$message('debug', paste("Processing entry", n, "/", length(entries), "..."))

			e.df <- NULL
			if ( ! is.null(e))
				e.df <- e$getFieldsAsDataFrame(only.atomic = only.atomic, compute = compute, fields = fields, flatten = flatten, only.card.one = only.card.one)
			else if (null.to.na)
				e.df <- data.frame(ACCESSION = NA_character_)

			if ( ! is.null(e.df))
				df.list <- c(df.list, list(e.df))
		}

		# Build data frame of all entries
		if ( ! is.null(df.list)) {
			.self$message('info', paste("Merging entries into a single data frame..."))
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
})

# Entries to JSON {{{1
################################################################

Biodb$methods( entriesToJson = function(entries, compute = TRUE) {
	":\n\nConvert a list of BiodbEntry objects into JSON. Returns a vector of characters."

	json <- vapply(entries, function(e) e$getFieldsAsJson(compute = compute), FUN.VALUE = '')

	return(json)
})

# Save entries as JSON {{{1
################################################################

Biodb$methods( saveEntriesAsJson = function(entries, files, compute = TRUE) {
	":\n\nSave a list of entries in JSON format."

	.self$.assert.equal.length(entries, files)

	# Save
	for (i in seq_along(entries)) {
		json <- entries[[i]]$getFieldsAsJson(compute = compute)
		writeChar(json, files[[i]])
	}

	return(json)
})

# Compute fields {{{1
################################################################

Biodb$methods( computeFields = function(entries) {
	":\n\nCompute missing fields in entries."

	# Loop on all entries
	for (e in entries)
		e$computeFields()
})

# Copy database {{{1
################################################################

Biodb$methods( copyDb = function(conn.from, conn.to) {
	":\n\nCopy all entries of a database into another database. The connector of the destination database must be editable."
	
	# Get all entry IDs of "from" database
	ids = conn.from$getEntryIds()

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
		lapply(.self$getObservers(), function(x) x$progress(type = 'info', msg = 'Copying entries.', index = i, total = length(ids), first = (i == 1)))
	}
})

# Show {{{1
################################################################

Biodb$methods( show = function() {
	cat("Biodb instance, version ", as.character(packageVersion('biodb')), ".\n", sep = '')
})

# PRIVATE METHODS {{{1
################################################################

# Check locale {{{2
################################################################

Biodb$methods( .check.locale = function() {

	# Get locale
	locale <- Sys.getlocale()
	locale.split <- strsplit(strsplit(Sys.getlocale(), ';')[[1]], '=')
	if (length(locale.split) == 1)
		LC_CTYPE <- locale.split[[1]][[1]]
	else {
		keys <- vapply(locale.split, function(x) x[[1]], FUN.VALUE = '')
		locale.values <- vapply(locale.split, function(x) x[[2]], FUN.VALUE = '')
		names(locale.values) <- keys
		LC_CTYPE <- locale.values[['LC_CTYPE']]
	}

	# Check LC_CTYPE
	if (length(grep('\\.utf-8$', tolower(LC_CTYPE))) == 0) {
		if (.self$.config$isEnabled('force.locale'))
			Sys.setlocale(locale = 'en_US.UTF-8') # Force locale
		else
			.self$message('warning', paste("LC_CTYPE field of locale is set to ", LC_CTYPE, ". It must be set to a UTF-8 locale like 'en_US.UTF-8'."))
	}
})

# DEPRECATED METHODS {{{1
################################################################

# Field is atomic {{{2
################################################################

Biodb$methods( fieldIsAtomic = function(field) {

	.self$.deprecated.method('BiodbEntryField::isVector()')

	return(.self$getEntryFields()$get(field)$isVector())
})

# Get field class {{{2
################################################################

Biodb$methods( getFieldClass = function(field) {

	.self$.deprecated.method('Biodb::getEntryFields()$get(field)$getClass()')

	return(.self$getBiodb()$getEntryFields()$get(field)$getClass())
})
