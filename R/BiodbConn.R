# vi: fdm=marker

# Class declaration {{{1
################################################################

BiodbConn <- methods::setRefClass("BiodbConn", contains = "BiodbObject", fields = list( .debug = "logical", .biodb = "ANY" ))

# Constructor {{{1
################################################################

BiodbConn$methods( initialize = function(debug = FALSE, biodb = NULL, ...) {
	is(biodb, "Biodb") || .self$message(MSG.ERROR, paste0("The biodb parameter must be of class Biodb, its class was ", class(biodb), "."))
	.biodb <<- biodb
	.debug <<- debug
	callSuper(...)
})

# Get biodb {{{1
################################################################

BiodbConn$methods( getBiodb = function() {
	return(.self$.biodb)
})

# Get entry content type {{{1
################################################################

BiodbConn$methods( getEntryContentType = function() {
	.self$.abstract.method()
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
	return(if (count) length(.self$getEntryIds()) else NA_integer_)
})
