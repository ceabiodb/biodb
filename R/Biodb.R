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
#' @param flatten     If set to \code{TRUE} and the field has a cardinality greater than one, then values are collapsed and output is a vector of class character. 
#' @param observers Either a \code{BiodbObserver} class instance or a list of \code{BiodbObserver} class instances.
#' @param only.atomic Set to \code{TRUE} if you want only the fields of atomic type (\code{integer}, \code{numeric}, \code{logical} and \code{character}) inside the data frame.
#' @param null.to.na  If \code{TRUE}, each \code{NULL} entry is converted into a line of \code{NA} values inside the data frame."
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{BiodbCache}}, \code{\link{BiodbConfig}}, \code{\link{BiodbObserver}}, \code{\link{BiodbLogger}}, \code{\link{BiodbEntryFields}}, \code{\link{BiodbDbsInfo}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create an instance without the default logger:
#' mybiodb <- biodb::Biodb(logger = FALSE)
#'
#' # Create an instance with a file logger
#' mybiodb <- biodb::Biodb(logger = FALSE, observers = biodb::BiodbLogger(file = "file.log"))
#'
#' @import methods
#' @include BiodbObject.R
#' @export Biodb
#' @exportClass Biodb
Biodb <- methods::setRefClass("Biodb", contains = "BiodbObject", fields = list( .factory = "ANY", .observers = "ANY", .config = "ANY", .cache = "ANY", .entry.fields = "ANY", .dbsinfo = "ANY" ))

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

	# Force locale to C
	if (.self$.config$isEnabled('force.c.locale'))
		Sys.setlocale(locale = 'C')
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

# Add observers {{{1
################################################################

Biodb$methods( addObservers = function(observers) {
	":\n\nAdd new observers. Observers will be called each time an event occurs. This is the way used in biodb to get feedback about what is going inside biodb code."

	# Check types of observers
	if ( ! is.list(observers)) observers <- list(observers)
	is.obs <- vapply(observers, function(o) is(o, "BiodbObserver"), FUN.VALUE = TRUE)
	if (any( ! is.obs))
		.self$message(MSG.ERROR, "Observers must inherit from BiodbObserver class.")

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

Biodb$methods( entriesToDataframe = function(entries, only.atomic = TRUE, null.to.na = TRUE, compute = TRUE) {
	":\n\nConvert a list of entries (\\code{BiodbEntry} objects) into a data frame."

	if ( ! is.list(entries))
		.self$message(MSG.ERROR, "Parameter 'entries' must be a list.")

	entries.df <- NULL

	if (length(entries) > 0) {

		.self$message(MSG.DEBUG, paste(length(entries), "entrie(s) to convert in data frame."))

		# Check classes
		if ( ! all(vapply(entries, function(x) is.null(x) || is(x, 'BiodbEntry'), FUN.VALUE = TRUE)))
			.self$message(MSG.ERROR, "Some objects in the input list are not a subclass of BiodbEntry.")

		# Loop on all entries
		n <- 0
		for (e in entries) {
			n <- n + 1
			.self$message(MSG.DEBUG, paste("Processing entry", n, "/", length(entries), "..."))
			if ( ! is.null(e)) {
				e.df <- e$getFieldsAsDataFrame(only.atomic = only.atomic, compute = compute)
				entries.df <- plyr::rbind.fill(entries.df, e.df)
			}
			else if (null.to.na) {
				e.df <- data.frame(accession <- NA_character_)
				colnames(e.df) <- 'ACCESSION'
				entries.df <- plyr::rbind.fill(entries.df, e.df)
			}
		}
	}

return(entries.df)
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
