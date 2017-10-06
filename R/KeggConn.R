# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include RemotedbConn.R
KeggConn <- methods::setRefClass("KeggConn", contains = "RemotedbConn", fields = list(.db.name = "character", .db.abbrev = "character"))

# Constructor {{{1
################################################################

KeggConn$methods( initialize = function(db.name = NA_character_, db.abbrev = NA_character_, ...) {

	callSuper(...)
	.self$.abstract.class('KeggConn')

	# Set name
	if (is.null(db.name) || is.na(db.name))
		.self$message('error', "You must set a name for this KEGG database.")
	.db.name <<- db.name

	# Set abbreviation
	if (is.null(db.abbrev) || is.na(db.abbrev))
		.self$message('error', "You must set an abbreviation for this KEGG database.")
	.db.abbrev <<- db.abbrev
})

# Complete entry id {{{1
################################################################

KeggConn$methods( .complete.entry.id = function(id) {
	return(paste(.self$.db.abbrev, id, sep = ':'))
})

# Get entry content url {{{1
################################################################

KeggConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {
	return(paste(.self$getBaseUrl(), 'get/', .self$.complete.entry.id(id), sep = ''))
})

# Get entry page url {{{1
################################################################

KeggConn$methods( getEntryPageUrl = function(id) {
	return(paste('http://www.genome.jp/dbget-bin/www_bget?', .self$.complete.entry.id(id), sep = ''))
})

# Get entry content {{{1
################################################################

KeggConn$methods( getEntryContent = function(entry.id) {

	# Initialize return values
	content <- rep(NA_character_, length(entry.id))

	# Request
	content <- vapply(entry.id, function(x) .self$.get.url(.self$getEntryContentUrl(x)), FUN.VALUE = '')

	return(content)
})

# Get entry ids {{{1
################################################################

KeggConn$methods( getEntryIds = function(max.results = NA_integer_) {

	# Get IDs
	ids <- .self$.get.url(paste(.self$getBaseUrl(), 'list/', .self$.db.name, sep = ''))

	# Extract IDs
	ids <- strsplit(ids, "\n")[[1]]
	#ids <- sub('^[^:]+:([^\\s]+)\\s.*$', '\\1', ids, perl = TRUE)
	ids <- sub('^[^:]+:([^\\s]+)\\s.*$', '\\1', ids, perl = TRUE)

	# Cut results
	if ( ! is.na(max.results) && max.results > 0)
		ids <- ids[1:max.results]

	return(ids)
})
