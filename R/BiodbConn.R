# vi: fdm=marker

# Class declaration {{{1
################################################################

BiodbConn <- methods::setRefClass("BiodbConn", contains = "BiodbObject", fields = list( .biodb = "ANY", .content.type = "character"))

# Constructor {{{1
################################################################

BiodbConn$methods( initialize = function(biodb = NULL, content.type = NA_character_, ...) {

	callSuper(...)

	# Set biodb
	if ( ! is(biodb, "Biodb"))
		.self$message(MSG.ERROR, paste0("The biodb parameter must be of class Biodb, its class was ", class(biodb), "."))
	.biodb <<- biodb

	# Set content type
	if (is.null(content.type) || is.na(content.type))
		.self$message(MSG.ERROR, "Content type not defined.")
	if ( ! content.type %in% BIODB.CONTENT.TYPES)
		.self$message(MSG.ERROR, paste("Unknown content type \"", content.type, "\"."))
	.content.type <<- content.type
})

# Get entry content type {{{1
################################################################

BiodbConn$methods( getEntryContentType = function(type) {
	return(.self$.content.type) 
})

# Get biodb {{{1
################################################################

BiodbConn$methods( getBiodb = function() {
	return(.self$.biodb)
})

# Get entry {{{1
################################################################

BiodbConn$methods( getEntry = function(id, drop = TRUE) {
	content <- .self$getEntryContent(id)
	return(.self$createEntry(content, drop = drop))
})

# Get entry content {{{1
################################################################

# Download entry content from the public database.
# type      The entry type.
# id        The ID of the entry to get.
# RETURN    An entry content downloaded from database.
BiodbConn$methods( getEntryContent = function(id) {
	.self$.abstract.method()
})

# Create entry from content {{{1
################################################################

# Creates a Compound instance from file content.
# content       A file content, downloaded from the public database.
# RETURN        A compound instance.
BiodbConn$methods( createEntry = function(content, drop = TRUE) {
	.self$.abstract.method()
})

# Get entry ids {{{1
################################################################

# Get a list of IDs of all entries contained in this database.
BiodbConn$methods( getEntryIds = function(max.results = NA_integer_) {
	.self$.abstract.method()
})

# Get nb entries {{{1
################################################################

# Get the number of entries contained in this database.
# count: if no straightforward way exists to get number of entries, count the output of getEntryIds().
BiodbConn$methods( getNbEntries = function(count = FALSE) {

	n <- NA_integer_

	if (count) {
		ids <- .self$getEntryIds()
		if ( ! is.null(ids))
			n <- length(ids)
	}

	return(n)
})
