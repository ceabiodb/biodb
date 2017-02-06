# vi: fdm=marker

# Class declaration {{{1
################################################################

#'The mother abstract class of all connection classes.
#'@export
Biodb <- methods::setRefClass("Biodb", contains = "BiodbObject", fields = list( .factory = "ANY", .observers = "ANY", .config = "ANY", .cache = "ANY" ))

# Constructor {{{1
################################################################

Biodb$methods( initialize = function(useragent = NA_character_, logger = TRUE, ...) {

	# Create config instance
	.config <<- BiodbConfig$new()

	# Set observers
	.observers <<- list(WarningReporter$new(), ErrorReporter$new())
	if (logger)
		.self$addObservers(BiodbLogger$new())

	# Create cache
	.cache <<- BiodbCache$new(biodb = .self)

	# Create factory
	.factory <<- BiodbFactory$new(biodb = .self)

	callSuper(...)
})

# Get biodb {{{1
################################################################

Biodb$methods( getBiodb = function() {
	return(.self)
})

# Get config {{{1
################################################################

Biodb$methods( getConfig = function() {
	return(.self$.config)
})

# Get cache {{{1
################################################################

Biodb$methods( getCache = function() {
	return(.self$.cache)
})

# Get factory {{{1
################################################################

Biodb$methods( getFactory = function() {
	return(.self$.factory)
})

# Add observers {{{1
################################################################

Biodb$methods( addObservers = function(obs) {

	# Check types of observers
	if ( ! is.list(obs)) obs <- list(obs)
	is.obs <- vapply(obs, function(o) is(o, "BiodbObserver"), FUN.VALUE = TRUE)
	if (any( ! is.obs))
		stop("Observers must inherit from BiodbObserver class.")

	# Add observers to current list (insert at beginning)
	.observers <<- if (is.null(.self$.observers)) obs else c(obs, .self$.observers)
})

# Get observers {{{1
################################################################

Biodb$methods( getObservers = function() {
	return(.self$.observers)
})

# Entries' field to vector or list {{{1
################################################################

Biodb$methods( entriesFieldToVctOrLst = function(entries, field, flatten = FALSE) {
	"Extract  of entries (BiodbEntry objects) into a data frame.
	entries: a list containing BiodbEntry objects.
	field:   
	flatten: if the field has a cardinality greater than one, then values are collapsed and output is a vector of class character. 
	return:  a vector or a list of values, depending on the field's type."

	val <- NULL

	# Vector
	if (.self$fieldIsAtomic(field) && (flatten || .self$getFieldCardinality(field) == BIODB.CARD.ONE)) {
		field.class = .self$getFieldClass(field)

		if (length(entries) > 0)
			val <- vapply(entries, function(e) if (e$hasField(field)) e$getFieldValue(field, flatten = flatten) else as.vector(NA, mode = field.class), FUN.VALUE = vector(mode = field.class, length = 1))
		else
			val <- vector(mode = field.class, length = 0)
	}

	# List
	else {
		if (length(entries) > 0)
			val <- lapply(entries, function(e) e$getFieldValue())
		else
			val <- list()
	}

	return(val)
})

# Get field class {{{1
################################################################

Biodb$methods( getFieldClass = function(field) {

	if ( ! field %in% BIODB.FIELDS[['name']])
		biodb$message(MSG.ERROR, paste("Unknown field \"", field, "\".", sep = ''))

	# Get class
	class <- BIODB.FIELDS[BIODB.FIELDS[['name']] == field, 'class']

	return(class)
})

# Get field class {{{1
################################################################

Biodb$methods( fieldIsAtomic = function(field) {
	return(.self$getFieldClass(field) %in% c('integer', 'double', 'character', 'logical'))
})

# Entries to data frame {{{1
################################################################

Biodb$methods( entriesToDataframe = function(entries, only.atomic = TRUE) {
	"Convert a list of entries (BiodbEntry objects) into a data frame.
	only.atomic Set to TRUE if you want only the atomic fields (integer, numeric, logical and character) inside the data frame."

	# Check classes
	all(vapply(entries, function(x) is(x, 'BiodbEntry'), FUN.VALUE = TRUE)) || .self$message(MSG.ERROR, "Some objects in the input list are not a subclass of BiodbEntry.")

	entries.df <- NULL

	# Loop on all entries
	n <- 0
	for (e in entries) {
		n <- n + 1
		.self$message(BIODB.DEBUG, paste("Processing entry", n, "/", length(entries), "..."))
		e.df <- e$getFieldsAsDataFrame(only.atomic = only.atomic)
		entries.df <- plyr::rbind.fill(entries.df, e.df)
	}

	return(entries.df)
})
