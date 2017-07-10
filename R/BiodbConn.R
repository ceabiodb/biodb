# vi: fdm=marker

# Class declaration {{{1
################################################################

#' The mother abstract class of all database connectors.
#'
#' @import methods
#' @include ChildObject.R
#' @export BiodbConn
#' @exportClass BiodbConn
BiodbConn <- methods::setRefClass("BiodbConn", contains = "ChildObject", fields = list(.id = "character", .content.type = "character", .base.url = "character"))

# Constructor {{{1
################################################################

BiodbConn$methods( initialize = function(id = NA_character_, content.type = NA_character_, base.url = NA_character_, ...) {

	callSuper(...)

	.id <<- id

	# Set content type
	if (is.null(content.type) || is.na(content.type))
		.self$message(MSG.ERROR, "Content type not defined.")
	if ( ! content.type %in% BIODB.CONTENT.TYPES)
		.self$message(MSG.ERROR, paste("Unknown content type \"", content.type, "\"."))
	.content.type <<- content.type

	# Set base URL
	if (is.na(base.url))
		base.url <- .self$getBiodb()$getDbsInfo()$get(.self$getId())$getBaseUrl()
	.self$.setBaseUrl(base.url)
})

# Get id {{{1
################################################################

BiodbConn$methods( getId = function() {
	return(.self$.id)
})

# Get base URL {{{1
################################################################

BiodbConn$methods( getBaseUrl = function() {
	return(.self$.base.url)
})

# Set base URL {{{1
################################################################

BiodbConn$methods( .setBaseUrl = function(base.url) {

	if (is.null(base.url) || any(is.na(base.url)))
		.self$message(MSG.ERROR, "Cannot set a NULL or NA URL to a database connector.")

	.base.url <<- base.url
})

# Get entry content type {{{1
################################################################

BiodbConn$methods( getEntryContentType = function(type) {
	return(.self$.content.type) 
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
